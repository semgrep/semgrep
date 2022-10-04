<?php

function foo($s) {
  ArgAssert::isString($s, "s");
  ArgAssert::isArray($s, 
                     array(1, 2));
}
