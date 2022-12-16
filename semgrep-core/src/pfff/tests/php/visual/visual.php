<?php

function test_variables_and_parameters($param1) {
  echo $param1;
  $local1 = 1;
  echo $local1;
  echo $undefined_variable;
}

class TestClass {
  public function amethod() {
  }
}

function test_closure() {
  $f = (function($a) {
      return 1 + $a;
    });
}


function test_string_use() {
  $a = "<html> is bad in string </html>";
  $xhp = <this-is-good>$a</this-is-good>;
  $url = $_SERVER['PHPROOT'] . "/path/to/url.php";
}


function test_dynamic_calls($b) {
  a_foo();
  $a = 'a_foo';
  $a();
  $b();
  call_user_func($a, array());
}

function test_dynamic_class() {
  $class = 'Foo';
  $o = new $class();
}

function test_call_a_dynamic_caller() {
  $b = 'b_foo';
  a_foo();
  dynamic_calls($b);
}



function test_take_arg2_by_ref($a, &$argref, $c) {
}

function test_byref_calls() {
  $a = 1;
  $b = 2;
  $c = 3;
  take_arg2_by_ref($a, $b, $c);
}
