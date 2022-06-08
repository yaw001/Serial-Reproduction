<?php
// {e,s,c,i,'available'}
//		=> {e,s,c,i,sid,'out'}
//			=> {e,s,c,i,sid,'done'}, {e,s,c,i+1,'available'}
//			=> {e,s,c,i,'available'}, {e,s,c,i,sid,'rejected'}
//			=> {e,s,c,i,'available'}, {e,s,c,i,sid,'abandoned'}
include('config.php');

$date_db = date('Y-m-d H:i:s');

// $_REQUEST['request'] = "{\"sid\":\"visitor-6x9uv\",\"expt\":\"debug\",\"trial_n\":2}";
$request = json_decode($_REQUEST['request'], true);
// $request = json_decode('{"sid":"visitor-ilfxk","expt":"tester","trial_n":1}', true);
// DL($request);

abandon_overdue($threshold);

if($request['trial_n'] < 0){
	try{
		$file = sprintf('%s/seed/%s.%d-%d-%d.json', DATADIR, 'practice', $request['trial_n'], 0, 0);
		$filecontents = json_decode(file_get_contents($file), true);
	} catch (Exception $e) {
		ajax_error('file_read', $e->getMessage());
	}
} else {
	$data = get_available($request['expt'], $request['sid']);
	// DL($data);

	// // check out row.
	$WHERE = array('expt'=>$data['expt'], 'seed'=>$data['seed'], 'chain'=>$data['chain'], 'iter'=>$data['max_iter'] , 'status'=>'available');
	$SET = array('status'=>'out', 'sid'=>$request['sid'], 'startDate'=>$date_db);
	update_iter($WHERE, $SET);

	try{
		$file = sprintf('%s/seed/%s.%d-%d-%d.json', DATADIR, $data['expt'], $data['seed'], $data['chain'], $data['max_iter']);
		$filecontents = json_decode(file_get_contents($file), true);
	} catch (Exception $e) {
		ajax_error('file_read', $e->getMessage());
	}
}

// send appropriate data back to client
ajax_return($filecontents);

