<?php

// ruleid:test-symmetric-matching-enabled
if (foo1($stuff) == foo2($stuff)) {
    // Do stuff
}

// ruleid:test-symmetric-matching-enabled
if (foo2($stuff) == foo1($stuff)) {
    // Do stuff
}

// ruleid:test-symmetric-matching-enabled
if (foo1($stuff) != foo2($stuff)) {
    // Do stuff
}

// ruleid:test-symmetric-matching-enabled
if (foo2($stuff) != foo1($stuff)) {
    // Do stuff
}

// ruleid:test-symmetric-matching-disabled
if (bar1($stuff) == bar2($stuff)) {
    // Do stuff
}

// ok:test-symmetric-matching-disabled
if (bar2($stuff) == bar1($stuff)) {
    // Do stuff
}

// ruleid:test-symmetric-matching-disabled
if (bar1($stuff) != bar2($stuff)) {
    // Do stuff
}

// ok:test-symmetric-matching-disabled
if (bar2($stuff) != bar1($stuff)) {
    // Do stuff
}
