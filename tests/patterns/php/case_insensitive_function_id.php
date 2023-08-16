<?php
function foo() {
    // Function references are not case sensitive.
    // MATCH:
    foo();
    // MATCH:
    Foo();
    // MATCH:
    FOO();
    return 1;
}
?>
