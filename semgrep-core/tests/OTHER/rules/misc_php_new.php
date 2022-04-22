<?php

function foo() {
    
$parts = explode("/", $_SERVER['PATH_INFO']);
$controllerName = $parts[0];

// ruleid: tainted-object-instantiation
$controller = new $controllerName($parts[1]);

// ruleid: tainted-object-instantiation
$controller = new MyController($controllerName);

$a = "MyController";
// ruleid: tainted-object-instantiation
$controller = new $a();

}
