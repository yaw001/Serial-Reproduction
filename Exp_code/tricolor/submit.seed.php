<?php
try {
	include('config.php');

	$data = json_decode($_REQUEST['data'], true);
	// $data = ['expt' => 'test', 'seed'=>0, 'chain'=>0, 'iter'=>0, 'stimuli'=>['fish!']];


	$sql = "SELECT expt, seed, chain, iter FROM $table WHERE expt='".$data['expt']."' AND seed=".$data['seed']." AND chain=".$data['chain']." AND iter=".$data['iter'].";";
// echo $sql . "\n\n";
	if(!($result = $conn->query($sql))){
		ajax_error('cannotquery', "Error add_iter: " . $conn->error . "\nSQL: " . $sql);
	}
	if($row = $result->fetch_assoc()){
		// row does exist...
		ajax_error('seedchain', 'expt-seed-chain-iter exists');
	} else {
		$rawfile = sprintf('%s/seed/%s.%d-%d-%d.json', DATADIR, $data['expt'], $data['seed'], $data['chain'], $data['iter']);
		$fp = fopen($rawfile, 'w');
		fwrite($fp, json_encode($data, JSON_PRETTY_PRINT));
		fclose($fp);

		$sql = "INSERT INTO $table (expt, seed, chain, iter, status, data, createDate) VALUES ('".$data['expt']."', ".$data['seed'].", ".$data['chain'].", ".$data['iter'].", 'available', '".json_encode($data['stimuli'])."', NOW());";
// echo $sql . "\n\n";
	if(!$conn->query($sql)){
		ajax_error('cannotquery', "Error add_iter: " . $conn->error . "\nSQL: " . $sql);
	} 
		ajax_return($data);
	}


} catch (Exception $e) {
	ajax_error('cannotrun', $e->getMessage());
}
