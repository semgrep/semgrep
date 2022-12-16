<?php

function test_foreach_arrow_list() {
  $xs = array();
  foreach($x as $k => list($pair1, $pair2)) {
    var_dump($k, $pair1, $pair2);
  }
}
