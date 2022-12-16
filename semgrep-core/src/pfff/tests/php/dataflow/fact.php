<?php

function fact() {
$x = 5;
$y = $x;
$z = 1;
while($y > 1) {
  $z = $z * $y;
  $y = $y - 1;
}
echo $z;
}
