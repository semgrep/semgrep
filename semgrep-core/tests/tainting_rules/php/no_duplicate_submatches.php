<?php
// https://github.com/returntocorp/semgrep/issues/3742

function f() {
    //ERROR:
    sink(
        //OK: no duplicate!
        "$foo" . 'aaa'
    );
}
