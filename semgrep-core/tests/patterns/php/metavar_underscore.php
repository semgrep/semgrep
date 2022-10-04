<?php

// ERROR: match
echo 'Hello ' . htmlspecialchars($_COOKIE["name"]) . '!';
$COOKIE = $_COOKIE;

// ok
$a = $_POST["arg"];
// ok
$b = $_GET["other"];
// ok
$c = $dict["dict_arg"];

// ok: checklist-cookie-fetch
# Preferred way to get cookie value
$foo->getCookie('value');

// ok: checklist-cookie-fetch
# Ex: Attempt to fetch the UserID cookie value. Note: The
# value returned isn't trusted and is forced to be an int.
$sId = intval( $foo->getCookie( 'UserID' ) );
?>

