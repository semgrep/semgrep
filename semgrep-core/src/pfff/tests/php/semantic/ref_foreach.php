<?php

$array = array(1,2,3);

foreach($array as $k) {
  $k = 42;
};
var_dump($array);

// this one will modify $array
foreach($array as &$k) {
  $k = 42;
};

var_dump($array);
