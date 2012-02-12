<?php include("shared.php"); ?>
<?php
if (!IsAdminLoggedIn())
{
	Redirect("logout.php");
}

?>
<h1>Stu's Pizza Shack Logged In Users</h1>
<a href="login.php">Return to status page</a><hr>
<table border=1 width=100%>
	<tr><td bgcolor="gray" width=25%><b>Username</b></td><td bgcolor="gray" width=25%><b>IP Address</b></td><td bgcolor="gray" width=25%><b># of Minutes Logged In</b></td><td bgcolor="gray" width=25%><b>Login Time</b></td></tr>
<?php
	
	$passhandle = fopen("/var/www/tmp/loggedin","r");
	while ($passinfo = fscanf($passhandle,"%s\t%s\t%s\t%s\n"))
	{
		list($ip, $username, $date, $time) = $passinfo;
		$timestring = $date;
		$timestring .= " ";
		$timestring .= $time;
		$timenorm = parseDate($timestring,"%m-%d-%Y %H:%M:%s");
		$span = round(((time() - (fixTime(strtotime($timenorm))))/60)+300,2);
		echo("<tr><td>$username</td><td>$ip</td><td>$span</td><td>$timenorm</td></tr>");
	}
?>
</table>
<hr>
