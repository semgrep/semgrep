<?php

// ERROR: foreach-issue
foreach ($stacks as $value) {
  do_something($value);
}

// ERROR: foreach-issue
foreach ($stacks2 as $value2) {
  $a = 'a';
  do_something($value2);
}

?>
