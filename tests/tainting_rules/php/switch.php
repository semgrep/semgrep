<?php

function f($arg) {
    switch ($arg) {
        case 0:
            $a = 0;
            break;
            //OK:
            sink($source);
        case 1:
            $b = 0;
        case 2:
            $c = 0;
            //ruleid: test-switch
            sink($source);
            break;
        default:
            $d = 0;
    }
}
