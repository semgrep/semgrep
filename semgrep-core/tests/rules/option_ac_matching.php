<?php
// https://github.com/returntocorp/semgrep/issues/3198

//ruleid:test-ac-matching
if (!check1($stuff) && !check2($stuff)) {
    // Do stuff
}

//ruleid:test-ac-matching
if (!check1($stuff) && !check2($stuff) && !check3($stuff)) {
    // Do stuff
}

//ruleid:test-ac-matching
if (!check3($stuff) && !check1($stuff) && !check2($stuff)) {
    // DO other stuff
}
