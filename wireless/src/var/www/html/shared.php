<?php
function Random_Password($length) {
    srand(date("s"));
    $possible_charactors = "abcdefghijklmnopqrstuvwxyz1234567890ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    $string = "";
    while(strlen($string)<$length) {
        $string .= substr($possible_charactors, rand()%(strlen($possible_charactors)),1);
    }
    return($string);
}

// from http://ragesw.com/forums/index.php?s=1a715033a01c093654e68491edcac9be&showtopic=8
function GetIP()
{
if (getenv("HTTP_CLIENT_IP") && strcasecmp(getenv("HTTP_CLIENT_IP"), "unknown"))
$ip = getenv("HTTP_CLIENT_IP");
else if (getenv("HTTP_X_FORWARDED_FOR") && strcasecmp(getenv("HTTP_X_FORWARDED_FOR"), "unknown"))
$ip = getenv("HTTP_X_FORWARDED_FOR");
else if (getenv("REMOTE_ADDR") && strcasecmp(getenv("REMOTE_ADDR"), "unknown"))
$ip = getenv("REMOTE_ADDR");
else if (isset($_SERVER['REMOTE_ADDR']) && $_SERVER['REMOTE_ADDR'] && strcasecmp($_SERVER['REMOTE_ADDR'], "unknown"))
$ip = $_SERVER['REMOTE_ADDR'];
else
$ip = "unknown";
return($ip);
}
function Redirect($url)
{
	$host  = $_SERVER['HTTP_HOST'];
	$uri  = rtrim(dirname($_SERVER['PHP_SELF']), '/\\');
	$extra = $url;
	$server = getServer();
	header("Location: $server$extra");
}

function LogToFile($message)
{
	$f = fopen("/var/www/tmp/logfile","a");
	$ip = GetIP();
	$time = date("m-d-Y H:i:s");
	$user = $_request['username'];
	fputs($f,"$message\t$ip\t$user\t$time\n");
	fclose($f);
}

// function taken from PHP documentation commentary online
// converts a date string to a standardized date string that PHP can then parse
function parseDate( $date, $format ) {
  // Builds up date pattern from the given $format, keeping delimiters in place.
  if( !preg_match_all( "/%([YmdHMsu])([^%])*/", $format, $formatTokens, PREG_SET_ORDER ) ) {
   return false;
  }
  foreach( $formatTokens as $formatToken ) {
   $delimiter = preg_quote( $formatToken[2], "/" );
   if($formatToken[1] == 'Y') {
     $datePattern .= '(.{1,4})'.$delimiter;
   } elseif($formatToken[1] == 'u') {
     $datePattern .= '(.{1,5})'.$delimiter;
   } else {
     $datePattern .= '(.{1,2})'.$delimiter;
   } 
  }

  // Splits up the given $date
  if( !preg_match( "/".$datePattern."/", $date, $dateTokens) ) {
   return false;
  }
  $dateSegments = array();
  for($i = 0; $i < count($formatTokens); $i++) {
   $dateSegments[$formatTokens[$i][1]] = $dateTokens[$i+1];
  }
 
  // Reformats the given $date into rfc3339
 
  if( $dateSegments["Y"] && $dateSegments["m"] && $dateSegments["d"] ) {
   if( ! checkdate ( $dateSegments["m"], $dateSegments["d"], $dateSegments["Y"] )) { return false; }
   $dateReformated =
     str_pad($dateSegments["Y"], 4, '0', STR_PAD_LEFT)
     ."-".str_pad($dateSegments["m"], 2, '0', STR_PAD_LEFT)
     ."-".str_pad($dateSegments["d"], 2, '0', STR_PAD_LEFT);
  } else {
   return false;
  }
  if( $dateSegments["H"] && $dateSegments["M"] ) {
   $dateReformated .=
     "T".str_pad($dateSegments["H"], 2, '0', STR_PAD_LEFT)
     .':'.str_pad($dateSegments["M"], 2, '0', STR_PAD_LEFT);
    
   if( $dateSegments["s"] ) {
     $dateReformated .=
       ":".str_pad($dateSegments["s"], 2, '0', STR_PAD_LEFT);
     if( $dateSegments["u"] ) {
       $dateReformated .=
       '.'.str_pad($dateSegments["u"], 5, '0', STR_PAD_RIGHT);
     }
   }
  }

  return $dateReformated;
}

// PHP4 is busted in ways PHP5 is not.
function fixTime($timeIntValue) 
{
	return $timeIntValue+39600;	// magic number
}

function getServer()
{
	return "http://192.168.1.1/";
}
function IsAlreadyLoggedIn() {
	$fc = file("/var/www/tmp/loggedin");
	$myip = GetIP();
	foreach($fc as $line)
	{
		if (strstr($line,$myip))
		{
			return true;
		}
	}
	return false;
}

function IsIPOnDenyList() {
	$fc = file("/var/www/tmp/denylist");
	$myip = GetIP();
	foreach ($fc as $line)
	{
		if (strstr($line, $myip))
		{
			return true;
		}
	}
	return false;
}
function GetLoggedInInfoString() {
	$fc = file("/var/www/tmp/loggedin");
	$myip = GetIP();
	foreach($fc as $line)
	{
		if (strstr($line,$myip))
		{
			list($ip,$name,$date,$time) = sscanf($line,"%s\t%s\t%s\t%s\n");
			
			return "You are logged in as user '$name'.  You have been connected since $date $time from IP address $ip.";
		}
	}
	return false;
}
function IsAdminLoggedIn() {
	$fc = file("/var/www/tmp/loggedin");
	$admins = file("/var/www/tmp/admins");
	
	$myip = GetIP();
	foreach($fc as $line)
	{
		if (strstr($line,$myip))
		{
			list($ip,$name,$date,$time) = sscanf($line,"%s\t%s\t%s\t%s\n");

			foreach($admins as $adm)
			{
				if (strcmp($name."\n",$adm)==0)
				{
					return true;
				}
			}
		}
	}
	return false;
}
?>
