<?php
// https://github.com/returntocorp/semgrep/issues/3741

//ERROR:
$a = "hello" . "foo" . "world";
//ERROR:
$aa = $hello . "foo" . $world;
//ERROR:
$b = "hello" . "world" . "foo" . "world";
//ERROR:
$bb = "hello" . $world . "foo" . "world";
//ERROR:
$c = "hello" . "world" . "foo" . "hello" . "world";
//ERROR:
$cc = "hello" . $world . "foo" . "hello" . $world;
