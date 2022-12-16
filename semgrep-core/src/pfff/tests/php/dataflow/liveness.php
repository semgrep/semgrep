<?php

// ./scheck -dataflow_pil tests/php/dataflow/liveness.php
function f() {
  $a = 1;
  $b = 2;
  $c = 3;
  y($a, $b);
  $d = 4;
  q($c);
}
