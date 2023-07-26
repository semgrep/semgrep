<?php
function foo() {

    # ruleid:test
    foo();

    # ruleid:test
    Foo();

    # ruleid:test
    FOO();

    # ok:test
    bar();

    return 1;
}
?>
