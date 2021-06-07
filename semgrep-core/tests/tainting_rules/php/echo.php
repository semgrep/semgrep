<?php
// https://github.com/returntocorp/semgrep/issues/3191

function foo() {
    //ERROR:
    echo $_GET['a'];

    //ERROR:
    echo custom($_GET['b']);

    //OK:
    echo htmlspecialchars($_GET['c']);

    //OK:
    echo 'hello';
}
