<?php

popen('/bin/ls', 'r');

popen('/bin' . '/ls', 'r');

$cmd = isset($_GET['cmd']) ? $_GET['cmd'] : '/bin/ls';
popen($cmd, 'r');

foo('abc');

foo('abc' . $cmd);
