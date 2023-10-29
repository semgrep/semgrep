<?php

// ruleid:test-commutative-matching-enabled
if (foo1($stuff) == foo2($stuff)) {
    // Do stuff
}

// ruleid:test-commutative-matching-enabled
if (foo2($stuff) == foo1($stuff)) {
    // Do stuff
}

// ruleid:test-commutative-matching-enabled
if (foo1($stuff) != foo2($stuff)) {
    // Do stuff
}

// ruleid:test-commutative-matching-enabled
if (foo2($stuff) != foo1($stuff)) {
    // Do stuff
}

// ruleid:test-commutative-matching-disabled
if (bar1($stuff) == bar2($stuff)) {
    // Do stuff
}

// ok:test-commutative-matching-disabled
if (bar2($stuff) == bar1($stuff)) {
    // Do stuff
}

// ruleid:test-commutative-matching-disabled
if (bar1($stuff) != bar2($stuff)) {
    // Do stuff
}

// ok:test-commutative-matching-disabled
if (bar2($stuff) != bar1($stuff)) {
    // Do stuff
}
