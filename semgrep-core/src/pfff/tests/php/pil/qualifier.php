<?php

// this could be transformed into
// $x = A::$var; $x['fld'] but it can not be transformed into
// $tmp = $var[...]; A::$tmp

A::$var['fld'];
