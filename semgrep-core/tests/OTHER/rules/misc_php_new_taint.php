<?php

$parts = explode("/", $_SERVER['PATH_INFO']);
$controllerName = $parts[0];

// ruleid: tainted-object-instantiation
$controller = new $controllerName($parts[1]);

// ok: tainted-object-instantiation
$controller = new MyController($controllerName);

// ok: tainted-object-instantiation
$a = "MyController";
$controller = new $a();
