<?php

function hello() {
  echo 'nothing';
}

//ERROR: match
function another($a = []) {
  do_something($a);
}

function another2($b) {
  echo 'a';
  do_something($b);
  return 'a';
}

//ERROR: match
function another3(array $c = []) {
  do_something($c);
}
