<?php

// ruleid:test-eq-is-symmetric
if (foo1($stuff) == foo2($stuff)) {
    // Do stuff
}

// ruleid:test-eq-is-symmetric
if (foo2($stuff) == foo1($stuff)) {
    // Do stuff
}

// ruleid:test-eq-is-symmetric
if (foo1($stuff) != foo2($stuff)) {
    // Do stuff
}

// ruleid:test-eq-is-symmetric
if (foo2($stuff) != foo1($stuff)) {
    // Do stuff
}
