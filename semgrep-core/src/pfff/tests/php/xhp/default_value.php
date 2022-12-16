<?php

class Foo {
  const CST = 1;
}

class :x:foo {
  attribute
    int padding = Foo::CST,
    Foo x = null
    ;
}


$x = <x:foo attr1="attr1" >xx</x:foo>;
var_dump($x);
