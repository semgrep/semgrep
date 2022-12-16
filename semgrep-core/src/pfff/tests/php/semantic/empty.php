<?php

// http://php.net/manual/en/function.empty.php
// empty() does not generate a warning if the variable does not exist.

// undefined variable error
if($foo) {
}

// no error
if (empty($foo)) {
}
