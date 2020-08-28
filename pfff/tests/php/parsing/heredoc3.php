<?php

$id = 42;

$x = <<<END
  This is $id
  This_is_$id
  This_is_$id_0
  This is {$id}
  This is {{$id}}
  This is ${id}
END;
var_dump($x);
