<?php

// another HPHP extension: user attributes.
// This is similar to http://en.wikipedia.org/wiki/Java_annotation

final class A { final public function foo() { /* .. */ } }
<< __MockClass >>
class MockD extends A { public function foo() { /* .. */ } }

<< __attribute >>
function foo() { }

// Multiple attributes with the same name are not allowed. The scalar values
// in attribute expressions are not allowed to contain user defined constants.
// User attributes can be applied to classes and functions and methods, but
// they cannot be applied to closures.

// you can also have attribute on parameters
function g(<<Data>> $a, <<Data>> String $b, $c) {
}
