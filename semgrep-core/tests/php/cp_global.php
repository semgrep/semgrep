<?php
// https://linear.app/r2c/issue/PA-870

$home="/home"."test";
$attachment="test";
$dir="test2";
$cmd= "sample string". escapeshellarg($dir) . "sample string2" . escapeshellarg($dir); 

//ERROR:
exec("rm $home");

//ERROR:
exec("tar -czf $attachment $dir");
//ERROR:
exec("rm -rf " . escapeshellarg($dir));

//ERROR:
$var= exec($cmd);
