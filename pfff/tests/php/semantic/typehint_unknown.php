<?php

function test(Foo $o) {
}

class Bar { }

$o = new Bar();
//HipHop Fatal error: Argument 1 passed to test() must be an instance of Foo
test($o);
