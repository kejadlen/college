<?php include("shared.php"); ?>
<?php

$myip = GetIP();

if (IsIPOnDenyList())
{
	Redirect("index.php?denylist=true");
	$myip = "invalid";
}

$fc = file("/var/www/tmp/noncefile");
$handle = fopen("/var/www/tmp/noncefile", "w");

$authok = false;

foreach($fc as $line)
{
	$userinfo = sscanf($line, "%s\t%s\t%s\t%s\n"); 
	list ($ip, $date,$time, $nonce) = $userinfo;
	$timestring = $date;
	$timestring .= " ";
	$timestring .= $time;
	$timenorm = parseDate($timestring,"%m-%d-%Y %H:%M:%s");
	
	$actualtime = strtotime($timenorm);
	$actualtime = fixTime($actualtime);
	$timenow = time();
	$allowabletime = $actualtime + 300;
	
	//echo("allowable: $allowabletime >= timenow: $timenow");
	if ($timenow <= $allowabletime)
	{
		if (strcmp($myip,$ip) == 0)
		{
			if (!$authok)
			{
			$passhandle = fopen("/var/www/tmp/passwordfile","r");
			while ($passinfo = fscanf($passhandle,"%s\t%s\n"))
			{
				list($username,$password) = $passinfo;
				if (strcmp($_REQUEST['username'],$username)==0)
				{
					$authstring = $password.$nonce;
					$md5val = md5($authstring);

					if ($md5val === $_REQUEST['results'])
					{
					
							LogToFile("Success");
					
							// Add to logged in file
							$f = fopen("/var/www/tmp/loggedin","a");
							$timestr = date("m-d-Y H:i:s");
							fputs($f,"$myip\t$username\t$timestr\n");
							fclose($f);
							
							system("sudo /root/src/add_ip ".$myip);

							$authok = true;
						
					}
				}
			}
			fclose($passhandle);
			}
			if (!$authok)
			{
				fputs($handle,$line);
			}
		}
	}
}
fclose($handle);
$server = getServer();
if (IsAlreadyLoggedIn())
{
	echo (GetLoggedInInfoString());
	echo('<form name="logout" action="');
	echo($server);
	echo('logout.php" method="post"><input type="submit" value="Logout"/></form>');
	
	if (IsAdminLoggedIn())
	{
		echo('<hr><h4>Admin Console</h4><ul><li><a href="useradmin.php">Manage Users</a></li><li><a href="currentusers.php">Currently Logged In Users</a></li><li><a href="denylist.php">Manage Deny List</a></li></ul>');
	}
}
else
{
	LogToFile("AuthFailure");
	if (strcmp($myip,"invalid")!= 0)
	{
		Redirect("index.php?invalid_password=true");
	}
	echo ("You are not logged in.  <a href='".$server."index.php'>Click Here</a> to try again.");

}

?>
