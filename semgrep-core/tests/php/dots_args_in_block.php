<?php

if (isset($_GET['q'])) {
  // ruleid:issue
  do_something($_GET['q']);
} else {
  $a = 'a';
}

if (isset($_GET['p'])) {
  $a = 2;
  // ruleid:issue
  do_something($_GET['p']);
} else {
  $a = 'a';
}

if (isset($_GET['p'])) {
  // ruleid:issue
  do_something($_GET['p']);
} else {
  // nothing
}

?>
