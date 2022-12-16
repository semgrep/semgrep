<?php

require_once 'require_module.php';

require_module('moda.php');

// note that xdebug traces include only function calls,
// and the location printed for each function call is the
// location at the call site. If require_ex() echo
// directly, then the trace will not contain any mention
// of require_ex.php. We need to have a function call inside
// require_ex() itself, hence the introduction of internal_require_ex()
//
function require_ex() {
  internal_require_ex();
}

function internal_require_ex() {
  echo "require_ex\n";
}
