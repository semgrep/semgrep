<?php

function require_module($f)
{
  echo "require_module $f\n";
  require_once $f;
}
