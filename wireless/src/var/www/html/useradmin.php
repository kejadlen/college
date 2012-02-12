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
	
	$fc=file("/var/www/tmp/passwordfile");

	//open same file and use "w" to clear file

	$f=fopen("/var/www/tmp/passwordfile","w");

	//loop through array using foreach

	foreach($fc as $line)
	{
	    if (!strstr($line,$_GET["username"])) //look for $key in each line
		{
			fputs($f,$line); //place $line back in file
		}
	}
	fclose($f);
}
if ($_REQUEST["action"] === "ADD")
{
	$f=fopen("/var/www/tmp/passwordfile","a");
	$usernameline=$_REQUEST["username"];
	$usernameline.="\t";
	$usernameline.=$_REQUEST["password"];
	
	fputs($f,"$usernameline\n");
	fclose($f);
}
}
?>
<h1>Stu's Pizza Shack User Admin</h1>
<a href="login.php">Return to status page</a><hr>
<table border=1 width=100%>
	<tr><td bgcolor="gray" width=40%><b>Username</b></td><td bgcolor="gray" width=40%><b>Password</b></td><td width=20%></td></tr>
<?php
	
	$passhandle = fopen("/var/www/tmp/passwordfile","r");
	while ($passinfo = fscanf($passhandle,"%s\t%s\n"))
	{
		list($username,$password) = $passinfo;
		echo("<tr><td>$username</td><td>$password</td><td><a href=");
		echo('"');
		echo("useradmin.php?action=REMOVE&username=$username&password=$password");
		echo('"');
		echo(">remove $username</a></td></tr>");
	}
?>
</table>
<hr>
<h4>Add New User:</h4>
<form method="post" action="useradmin.php" name="newuser">
	username: <input type="text" name="username">
	<br>password: <input type="password" name="password">
	<input type="hidden" name="action" value="ADD">
	<input value="Add User" type="submit">
</form>
