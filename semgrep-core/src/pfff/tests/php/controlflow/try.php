<?php

function foo(){
  try {
    return 1;
  } catch (Exception $e) {
    throw new Exception();
  }
}
