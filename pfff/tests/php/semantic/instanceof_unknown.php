<?php

class Bar {
}

$o = new Bar();

// This is ok, and it's ok to be ok. PHP is kinda right here
// If the class does not exist at runtime, no objects of this class
// could have ever been created, so it's safe for instanceof
// to return true instead of throwing Unknown Class.
if($o instanceof Foo) {
  echo "instance of Foo\n";
}
