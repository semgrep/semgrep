<?php
function foo() {
    $a = 1;
    $b = 2;
    //ERROR:
    if ($a+$b == $a+$b) {
        return 1;
    }
    return 0;
}

?>
