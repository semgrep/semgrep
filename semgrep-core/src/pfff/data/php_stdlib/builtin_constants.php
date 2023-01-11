<?php

$arr = get_defined_constants();
ksort($arr);

echo "<?php\n";
foreach ($arr as $k => $v) {
  $v = $v ? $v : -1;
  echo "define('$k', ";
  var_export($v);
  //echo "0";
  echo ");\n";
}
