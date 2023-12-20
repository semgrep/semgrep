<?php

// ruleid:test-eq-is-not-symmetric
if (bar1($stuff) == bar2($stuff)) {
    // Do stuff
}

// ok:test-eq-is-not-symmetric
if (bar2($stuff) == bar1($stuff)) {
    // Do stuff
}

// ruleid:test-eq-is-not-symmetric
if (bar1($stuff) != bar2($stuff)) {
    // Do stuff
}

// ok:test-eq-is-not-symmetric
if (bar2($stuff) != bar1($stuff)) {
    // Do stuff
}
