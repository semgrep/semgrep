<?php

function test1() {
    //ERROR: match
    if ($a > 2) {
        return 0;
    }
    if ($a < 2) {
        return 1;
    }
    return 0;
}
