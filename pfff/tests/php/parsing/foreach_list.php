<?php

$xs = array(Pair {'a', 1}, Pair{'b',2});

foreach($xs as list($x, $y)) {
  var_dump($x, $y);
}