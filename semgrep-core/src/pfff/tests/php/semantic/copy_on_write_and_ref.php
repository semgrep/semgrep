<?php

$x = array(array(1,2,3), array(4,5,6));
// a deep copy is not done when one of the element had a refcount >
// 1. It's probably because Zend developer didn't know how to deep
// copy cyclic structures and so each time there was a refcount > 1,
// which may indicate a cyclic structure (but it can be something
// else, such as a regular shared ref), then they would just stop (the
// right thing being, as in OCaml, to just traverse the graph and
// remember if one had already visited a node and what was the new
// corresponding pointer to reference again)

// taking a ref to a subpart of $x should increment the refcount
$z =& $x[1][0];

$y = $x;

$x[0][0] = 42;
$x[1][0] = 42;

// will print 1,2,3,42,5,6 :(
var_dump($y);
