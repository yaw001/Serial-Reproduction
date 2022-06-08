<?php
define('DEBUG', TRUE);
define('DATADIR', 'data/');

if(DEBUG){
	error_reporting(E_ALL);
	ini_set('display_errors', TRUE);
	ini_set('display_startup_errors', TRUE);
}

$hostname = "mysql.evullab.org";
$username = "evullab_expts";
$password = "evullabexpts3509mandler";
$database = "evullab_expts";

$table = 'iterated_tricolor';
$threshold = 10*60; // time limit to call something abandoned (in seconds)

function DL($message){
	if(DEBUG){
		print_r($message);
		echo "\n\n";
	}
}

function ajax_error($code, $message){
	header('content-type: application/json; charset=utf-8');
	header("access-control-allow-origin: *");
	$output = array('Success' => false,
			   		'Error' => $code,
			   		'Message' => $message);
	echo json_encode($output);
	die();
}


function ajax_return($contents){
	header('content-type: application/json; charset=utf-8');
	header("access-control-allow-origin: *");
	$output = array('Success' => true,
			   		'Data' => $contents);
	echo json_encode($output);
	die();
}

$conn = new mysqli($hostname, $username, $password, $database);

if($conn -> connect_error){ ajax_error("dbcon", "Connection failed: " . $conn->connect_error); }

function add_iter($expt, $seed, $chain, $iter, $status, $sid, $error, $data, $startDate, $lastDate, $note){
	global $conn;
	global $table;

	$sql = sprintf("INSERT INTO %s (expt, seed, chain, iter, status, sid, error, data, createDate, startDate, lastDate, note) 
		VALUES('%s', %d, %d, %d, '%s', '%s', %f, '%s', NOW(), '%s', '%s', '%s');", $table, $expt, $seed, $chain, $iter, $status, $sid, $error, $data, $startDate, $lastDate, $note);
	if(!$conn->query($sql)) {
	    ajax_error('additer', "Error add_iter: " . $conn->error . "\nSQL: " . $sql);
	}
}

function update_iter($WHERE, $SET){
	global $conn;
	global $table;

	$joined = array();
	foreach($WHERE as $field => $value){
		if(is_string($value)){
			$joined[] = "$field='".$value."'";
		} else {
			$joined[] = "$field=$value";
		}
	}
	$wsql = implode(" AND ", $joined);

	$joined = array();
	foreach($SET as $field => $value){
		if(is_string($value)){
			$joined[] = "$field='".$value."'";
		} else {
			$joined[] = "$field=$value";
		}
	}
	$ssql = implode(", ", $joined);

	$sql = "UPDATE $table SET $ssql, lastDate=NOW() WHERE $wsql";
	if(!$conn->query($sql)) {
	    ajax_error('update', "Error update_iter: " . $conn->error . "\nSQL: " . $sql);
	}
}

function abandon_overdue($threshold){
	global $conn;
	global $table;

	//		A. check if any iters are out that should be abandoned, abandon
	$sql = "SELECT expt, seed, chain, iter, sid, startDate 
				FROM $table 
				WHERE status='out' AND 
					TIME_TO_SEC(TIMEDIFF(NOW(), startDate)) > $threshold;";
	if (!($result = $conn->query($sql))) {
		ajax_error('selectout', "Error abandon_overdue 1: " . $conn->error . "\nSQL: " . $sql);
	} else {
		while($row = $result->fetch_assoc()){
			add_iter($row['expt'], $row['seed'], $row['chain'], $row['iter'], 'abandoned', $row['sid'], '', '', $row['startDate'], date('Y-m-d H:i:s'), '');
		}
	}

	$sql = "UPDATE $table 
				SET status='available', sid=''
				WHERE status='out' AND 
					TIME_TO_SEC(TIMEDIFF(NOW(), startDate)) > $threshold;";
	if (!$conn->query($sql)) {
	    ajax_error('updateout', "Error abandon_overdue 2: " . $conn->error . "\nSQL: " . $sql);
	}
}

function get_available($expt, $sid){
	global $conn;
	global $table;

	//		C. - if trialN >=3:
	//			identify seed/chain that:
	//			- sid has not done
	//			- does not have an iter out.

	// find seed, chain, where none are out
	$sql = "SELECT expt, seed, chain, data, MAX(iter) as max_iter 
			FROM
				(SELECT expt, seed, chain, iter, status, sid, data 
					FROM $table AS t1 
					WHERE 
						t1.status = 'available' AND
						t1.expt = '".$expt."' AND
						NOT EXISTS (SELECT expt,seed,chain FROM $table AS t2 
										WHERE t2.expt=t1.expt AND 
												t2.seed=t1.seed AND 
												t2.chain=t1.chain AND 
												t2.status='out') AND 
						NOT EXISTS (SELECT expt,seed,chain FROM $table as t3 
										WHERE t3.expt=t1.expt AND 
											  t3.seed=t1.seed AND 
												t3.chain=t1.chain AND 
												t3.sid='".$sid."')
					ORDER BY iter DESC) AS t 
			GROUP BY t.seed, t.chain
			ORDER BY max_iter, chain;";
	// DL($sql);
	if(!($result = $conn->query($sql))){
	 ajax_error('queryavail', "Error get_available: " . $conn->error . "\nSQL: " . $sql);
	}
	if(!($data = $result->fetch_assoc())){
		ajax_error('noavail', "no seed-chain selected as available for expt: " . $expt . ", sid: " . $sid);
	} else {
		return($data);	
	}
}

?>
