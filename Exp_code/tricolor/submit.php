<?php
include('config.php');

//$_REQUEST['data'] = "{\"trial_n\":0,\"sid\":\"visitor-3fqfn\",\"expt\":\"practice\",\"phase\":\"feedback\",\"error_threshold\":0.4,\"responses\":[{\"type\":\"circle\",\"center\":{\"x\":0.7106754280546324,\"y\":0.44638569735863076},\"radius\":0.03,\"color\":\"#4cbf40\",\"border\":{\"width\":0.005,\"color\":\"#000000\"}},{\"type\":\"circle\",\"center\":{\"x\":0.07539488419949186,\"y\":0.36418603311088565},\"radius\":0.03,\"color\":\"#404cbf\",\"border\":{\"width\":0.005,\"color\":\"#000000\"}},{\"type\":\"circle\",\"center\":{\"x\":-0.10510646602759106,\"y\":-0.7078155533131113},\"radius\":0.03,\"color\":\"#bf404d\",\"border\":{\"width\":0.005,\"color\":\"#000000\"}}],\"state\":{\"nReported\":3,\"selected\":null,\"start\":\"2018-04-20T18:24:09.161Z\",\"reporting\":null,\"displayTime\":0},\"stimuli\":[{\"type\":\"circle\",\"center\":{\"x\":-0.41614750372524467,\"y\":-0.0835597199826448},\"radius\":0.03,\"color\":\"#4cbf40\",\"border\":{\"width\":0.005,\"color\":\"#000000\"}},{\"type\":\"circle\",\"center\":{\"x\":0.2311002331881354,\"y\":-0.7421825205859172},\"radius\":0.03,\"color\":\"#404cbf\",\"border\":{\"width\":0.005,\"color\":\"#000000\"}},{\"type\":\"circle\",\"center\":{\"x\":-0.05813326277859168,\"y\":0.5149784927682739},\"radius\":0.03,\"color\":\"#bf404d\",\"border\":{\"width\":0.005,\"color\":\"#000000\"}}],\"seed\":0,\"chain\":0,\"iter\":0,\"settings\":{\"name\":\"debug\",\"maxTrials\":6,\"debug\":true,\"error_threshold\":0.4,\"error_match\":\"color\",\"readURL\":\"https://experiments.evullab.org/iterated_tricolor_debug/request.php\",\"saveURL\":\"https://experiments.evullab.org/iterated_tricolor_debug/submit.php\",\"timing\":{\"consent\":null,\"instructions\":null,\"presentation\":2000,\"mask\":300,\"response\":null,\"feedback\":null},\"default\":{\"n\":15,\"practice\":[3,9],\"colors\":[0,0.333,0.666],\"coffset\":0.317,\"type\":\"circle\",\"environment\":{\"type\":\"circle\",\"center\":{\"x\":0,\"y\":0},\"radius\":0.95,\"color\":\"#FFFFFF\",\"border\":{\"width\":0.01,\"color\":\"#888888\"}},\"circle\":{\"type\":\"circle\",\"center\":{\"x\":null,\"y\":null},\"radius\":0.03,\"color\":\"#333333\",\"border\":{\"width\":0.005,\"color\":\"#000000\"}},\"response\":{\"xy\":true,\"angle\":false,\"color\":false,\"size\":false,\"aspect_ratio\":false}}},\"error\":1.1953956986773346,\"score\":0}";

$data = json_decode($_REQUEST['data'], true);

$date_db = date('Y-m-d H:i:s');
$date_file = date('Y-m-d.His');

$errors = array();

// record raw file.
try{
	$data['submit_time'] = $date_db; // update with proper iso format
	$rawfile = sprintf('%s/submitted/%s.%d-%d-%d.%s.%s.json', DATADIR, $data['expt'], $data['seed'], $data['chain'], $data['iter'], $data['sid'], $date_file);
	$fp = fopen($rawfile, 'w');
	fwrite($fp, json_encode($data));
	fclose($fp);
} catch (Exception $e) {
	$errors['file_write_submit'] = $e->getMessage();
}

try{
	$sql = sprintf("SELECT * FROM %s WHERE expt='%s' AND seed=%d AND chain=%d AND iter=%d AND sid='%s' AND status='out';", $table, $data['expt'], $data['seed'], $data['chain'], $data['iter'], $data['sid']);
	if(!($result = $conn->query($sql))){
		$errors['sql_find'] = "Error submit: " . $conn->error . "\nSQL: " . $sql;
	} else {
		// query succeeded.
		if($row = $result->fetch_assoc()){
		// good, row exists...
			if($data['error'] < $data['error_threshold']){
				// GOOD, accept submission as new iteration.
				$nextout = Array();
				$nextout['expt'] = $data['expt'];
				$nextout['seed'] = $data['seed'];
				$nextout['chain'] = $data['chain'];
				$nextout['iter'] = $data['iter']+1;
				$nextout['stimuli'] = $data['responses'];

				// save new iter data as text to accepted folder
				try{
					$writefile = sprintf('%s/seed/%s.%d-%d-%d.json', DATADIR, $nextout['expt'], $nextout['seed'], $nextout['chain'],  $nextout['iter']);
					$fp = fopen($writefile, 'w');
					fwrite($fp, json_encode($nextout));
					fclose($fp);
				} catch (Exception $e) {
					$errors['file_write_iter'] = $e->getMessage();
				}

				add_iter($nextout['expt'], $nextout['seed'], $nextout['chain'], $nextout['iter'], 'available', '', 0, json_encode($nextout['stimuli']), '', '', '');

				$WHERE = array('expt'=>$data['expt'], 'seed'=>$data['seed'], 'chain'=>$data['chain'], 'iter'=>$data['iter'] , 'sid'=>$data['sid'], 'status'=>'out');
				$SET = array('status'=>'done', 'lastDate'=>$date_db, 'error'=>$data['error']);
				update_iter($WHERE, $SET);

				ajax_return(array('status'=>'accepted'));

			} else {
				// REJECT!
				// // update status of checked out iter to 'available'
				// add entry for new iteration

				add_iter($data['expt'], $data['seed'], $data['chain'], $data['iter'], 'rejected', $data['sid'], $data['error'], json_encode($data['responses']), $row['startDate'], $date_db, '');


				$WHERE = array('expt'=>$data['expt'], 'seed'=>$data['seed'], 'chain'=>$data['chain'], 'iter'=>$data['iter'] , 'sid'=>$data['sid'], 'status'=>'out');
				$SET = array('status'=>'available', 'sid'=>'', 'startDate'=>'');
				update_iter($WHERE, $SET);

				ajax_return(array('status'=>'rejected'));
			}
		} else {
		// row does not exist.
		// possibilities: out -> abandoned
		// expt: pilot, generated 
			add_iter($data['expt'], $data['seed'], $data['chain'], $data['iter'], 'independent', $data['sid'], $data['error'], json_encode($data['responses']), '', $date_db, '');

			ajax_return(array('status'=>'independent'));
		}
	}
} catch (Exception $e) {
	$errors['updates'] = $e->getMessage();
}

if(count($errors)>0){
	ajax_error('submit', print_r($errors, true));
}