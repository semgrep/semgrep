<?php
// https://github.com/returntocorp/semgrep/issues/3198

//ERROR:
if (!check1($stuff) && !check2($stuff)) {
    // Do stuff
}

//ERROR:
if (!check1($stuff) && !check2($stuff) && !check3($stuff)) {
    // Do stuff
}

//ERROR:
if (!check3($stuff) && !check1($stuff) && !check2($stuff)) {
    // DO other stuff
}

