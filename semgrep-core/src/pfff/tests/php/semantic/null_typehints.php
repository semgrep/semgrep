<?php

class Foo {
}

function bar(Foo $o) {
  var_dump($o);
}

function bar2(Foo $o = null) {
  var_dump($o);
}

// this will generate an error, which is good
bar(null);

// this is ok
bar2(null);
