<?php

function test_for_continue() {
  $truth_list = array();
  $actual_list = array();
  for ($i = 0; $i < count($truth_list); $i++) {
    $true_parse = $truth_list[$i]['semantic'];
    $attempted_parse = $actual_list[$i]['semantic'];

    if ($true_parse === $attempted_parse) {
      // This is not the mistaken parse.
      continue;
    }

    $whatever = 1;
    if($whatever) {
      return 'here';
    } else {
      return 'there';
    }
  }
}
