<?php

function foo() {

  if (!IS_INTERNAL_SANDBOX || $build_status === true) {
    return;
  }
}
