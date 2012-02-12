<?php include("shared.php"); ?>
<?php
if (!IsAdminLoggedIn())
{
	Redirect("logout.php");
}
else
{
if ($_GET["action"] === "REMOVE")
{
	
	$fc=file("/var/www/tmp/denylist");

	//open same file and use "w" to clear file

	$f=fopen("/var/www/tmp/denylist","w");

	//loop through array using foreach

	foreach($fc as $line)
	{
	    if (!strstr($line,$_GET["ip"])) //look for $key in each line
		{
			fputs($f,$line); //place $line back in file
		}
	}
	fclose($f);
}
if ($_REQUEST["action"] === "ADD")
{
	$f=fopen("/var/www/tmp/denylist","a");
	$line=$_REQUEST["ip"];
	
	fputs($f,"$line\n");
	fclose($f);
	
	$myip = GetIP();
	
	if (strcmp($myip,$_REQUEST["ip"]) == 0)
	{
		Redirect("logout.php");
	}
	
}
}
?>
<h1>Stu's Pizza Shack IP Deny List</h1>
<a href="login.php">Return to status page</a><hr>
<table border=1 width=100%>
	<tr><td bgcolor="gray" width=40%><b>IP</b></td><td width=20%></td></tr>
<?php
	
	$passhandle = fopen("/var/www/tmp/denylist","r");
	while ($passinfo = fscanf($passhandle,"%s\n"))
	{
		list($ip) = $passinfo;
		echo("<tr><td>$ip</td><td><a href=");
		echo('"');
		echo("denylist.php?action=REMOVE&ip=$ip");
		echo('"');
		echo(">remove $ip</a></td></tr>");
	}
?>
</table>
<hr>
<h4>Add New Deny Entry:</h4>
<form method="post" action="denylist.php" name="newhost">
	ip: <input type="text" name="ip">
	<br>
	<input type="hidden" name="action" value="ADD">
	<input value="Add User" type="submit">
</form>
