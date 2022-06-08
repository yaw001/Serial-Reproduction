<?php
include('config.php');

echo "<!DOCTYPE HTML>";
echo "<html><body>";

$sql = "SELECT status, COUNT(*) as num FROM iterated_tricolor WHERE expt='live' GROUP BY status;";
if($result = $conn->query($sql)){
	echo "<table>\n<tr><th>status</th><th>count</th></tr>\n";
	while($row = $result->fetch_assoc()){
		echo "<tr><td>".$row['status']."</td><td>".$row['num']."</td></tr>\n";
	}
	echo "</table>\n";
}

echo "</body></html>";