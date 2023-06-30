<?php
function foo(string $a, int $b) {
    # ruleid: no-string-eqeq
    echo $a == "hello";
    # ruleid: no-string-eqeq
    echo "hello" == $a;
    # ok: no-string-eqeq
    echo $b == 2;
    # ok: no-string-eqeq
    echo null == "hello";
    # ok: no-string-eqeq
    echo "hello" == null;
}
