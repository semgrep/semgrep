<?php

if (isset($_GET['q'])) {
  // ERROR: match
  do_something($_GET['q']);
} else {
  $a = 'a';
}

if (isset($_GET['p'])) {
  $a = 2;
  // ERROR: match
  do_something($_GET['p']);
} else {
  $a = 'a';
}

if (isset($_GET['p'])) {
  // ERROR: match
  do_something($_GET['p']);
} else {
  // nothing
}

?>
