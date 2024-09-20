<?php
// https://github.com/returntocorp/semgrep/issues/3742

function f() {
    //ruleid: duplicate-taint-findings
    sink(
        //OK: no duplicate!
        "$foo" . 'aaa'
    );
}
