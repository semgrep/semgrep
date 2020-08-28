<?php

class :x:frag {
  attribute string id;
}

$x = <x:frag
   id="foo" // This is the id
   />;

var_dump($x);

