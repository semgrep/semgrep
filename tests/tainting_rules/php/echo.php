<?php
// https://github.com/returntocorp/semgrep/issues/3191

function foo() {
    //ruleid: test-taint-echo
    echo $_GET['a'];

    //ruleid: test-taint-echo
    echo custom($_GET['b']);

    //OK:
    echo htmlspecialchars($_GET['c']);

    //OK:
    echo 'hello';
}
