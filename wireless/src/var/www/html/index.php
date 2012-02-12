<?php include("shared.php"); ?>
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
<head><meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />

<meta name="Generator" content="iWeb 1.0.0" />
<title>Welcome</title>

<link rel="stylesheet" type="text/css" media="screen" href="<?php echo(getServer())?>Welcome_files/Welcome.css" />
<script type="text/javascript" src="<?php echo(getServer())?>Welcome_files/Welcome.js"></script></head>
<body style="background: #000000; margin: 0pt; " onload="onPageLoad();">
<div style="text-align: center; "><div style="margin-left: auto; margin-right: auto; overflow: hidden; position: relative;  background: #000000; height: 794px; width: 700px; " id="bodyContent"><div style="height: 46px; left: 0px; position: absolute; top: 0px; width: 700px; " id="nav_layer"><div style="background: transparent; border: 1px #000000 none; float: none; height: 46px; left: 0px; margin: 0px; position: absolute; top: 0px; width: 700px; z-index: 1; "><div style="color: white; font-family: Arial, Helvetica, sans-serif; font-size: 11pt; background: transparent no-repeat scroll center; height: 26px; left: 205px; position: absolute; top: 17px; width: 290px; z-index: 1; " id="id1"><?php
if ($_GET['invalid_password']==="true")
{
	echo("Invalid Password;  Please Try Again.");
}
else if ($_GET['denylist']==="true")
{
	echo("<blink>Your IP is on the deny list.  DENIED.</blink>");
}
else
{
	
}
?></div>
</div>
</div>
<div style="height: 598px; left: 0px; position: absolute; top: 46px; width: 700px; " id="body_layer"><div style="background: transparent; border: 1px #000000 none; float: none; margin: 0px;  height: 49px; left: 35px; position: absolute; top: 22px; width: 630px; z-index: 1; " id="id2"><div><div><div class="graphic_shape_layout_style_default"><div class="paragraph Title" style="line-height: 42px; padding-bottom: 0pt; padding-top: 0pt; ">Welcome to Stu’s Pizza Shack</div>
</div>
</div>
</div>
</div>
<script type="text/javascript">
	function validateOnSubmit()
	{ 
		var passwordsource;
		var hash_value;
		passwordsource = document.login.password.value + document.login.nonce.value;
		hash_value = hex_md5(passwordsource);
		document.login.results.value = hash_value;
		document.login.password.value = "";
	}
</script>
<script type="text/javascript" src="include.js"/>
<?php 
	function Store_To_Nonce_File() {
		$nonce = Random_Password(16);
		$ip = GetIP();
		$xyz = time();
		$time = date("m-d-Y H:i:s");
		
		$out = fopen("/var/www/tmp/noncefile", "a");
		fputs($out,"$ip\t$time\t$nonce\n");
		fclose($out);
		return ($nonce);
	}
	echo("TRY_LOGIN");
	if (IsAlreadyLoggedIn())
	{
	echo("IS_LOGIN");
		$server = getServer(); 
		Redirect("login.php");
	}
?>
<img src="<?php echo(getServer())?>Welcome_files/shapeimage_1.png" alt="" id="id3" style="height: 1px; left: 35px; position: absolute; top: 1px; width: 630px; z-index: 1; " /><img src="<?php echo(getServer())?>Welcome_files/skd267745sdc-1.png" alt="" id="id4" style="border: none; height: 411px; left: 35px; position: absolute; top: 88px; width: 630px; z-index: 1; " /><div style="background: transparent; border: 1px #000000 none; float: none; margin: 0px;  height: 72px; left: 205px; position: absolute; top: 526px; width: 289px; z-index: 1; " id="id5"><div><div><form method="post" name="login" onsubmit="validateOnSubmit()" action="<?php echo(getServer())?>login.php"><div style="margin: 4px; "><div class="paragraph Heading_1" style="line-height: 25px; padding-top: 0pt; ">Username: <input type="text" name="username" size=16/></div>
<div class="paragraph Heading_1" style="line-height: 25px; padding-bottom: 0pt; ">Password: <input type="password" name="password" size=16/></div><div class="paragraph Heading_1" style="line-height: 25px; padding-bottom: 0pt; text-align: center; "><input type="hidden" name="nonce" value="<?php echo Store_To_Nonce_File(); ?>"/><input type="hidden" name="results"/><input type="submit" value="Login"/></div>
</div></form>
</div>
</div>
</div>
</div>
<div style="height: 150px; left: 0px; position: absolute; top: 644px; width: 700px; " id="footer_layer"><a href="http://www.mac.com" title="http://www.mac.com"><img src="<?php echo(getServer())?>Welcome_files/image.png" alt="Made on a Mac" id="id6" style="border: none; height: 50px; left: 280px; position: absolute; top: 80px; width: 139px; z-index: 1; " /></a></div>
</div>
</div>
</body>
</html>
