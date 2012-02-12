<?php include("shared.php"); ?>
<?php
	$fc=file("/var/www/tmp/loggedin");

//open same file and use "w" to clear file

$f=fopen("/var/www/tmp/loggedin","w");

//loop through array using foreach

$myip = GetIP();
$logouthappened=false;
foreach($fc as $line)
{
    if (!strstr($line,$myip)) //look for $key in each line
	{
		fputs($f,$line); //place $line back in file
	}
	else
	{
		$lineinfo = sscanf($line,"%s\t%s\t%s\t%s\n");
		list($ip,$username,$time,$date) = $lineinfo;
		$logouthappened=true;
		system("sudo /root/src/rm_ip ".$ip);
		echo("User '$username' logged out from IP address $ip on $date $time.<br>  <a href='".getServer()."index.php'>Click here</a> to log in again.");
	}
}
fclose($f);
if (!$logouthappened)
{
	echo("You are not logged in.  <a href='".getServer()."index.php'>Click here</a> to log in.");
}

?>
