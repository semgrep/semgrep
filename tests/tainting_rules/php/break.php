<?php

function f() {
    $arr = array(1,2,3,4,5);
    foreach ($arr as $i) {
        $bad = "ok";
        foreach ($arr as $j) {
            foreach ($arr as $k) {
                $bad = $source;
                if ($val == 2) {
                    break 2; 
                    //OK:
                    sink($source);
                }
            }
        }
        //ruleid: test-multi-break
        sink($bad);
    }
}
