<?php

# ruleid:doc-string-comment
/**
 * The first docstring
 * And some more
 */
function sayWelcomeMessage() {
    echo "welcome world!";
}

class A {
    # ruleid:doc-string-comment
    /**
     * This considered part of the doc-comment.
     */
    function method1() {
        echo "Go";
    }

    # ruleid:doc-string-comment
    /**
     * Another match
     */
    public function m2() {
        echo "Test";
    }
}

# ruleid:doc-string-comment
/**
 * With attributes
 */
#[ThisIsOk]
function attrFunc() {
    echo "Test";
}

function thisOneHasNoDocString() {
    echo "Test";
}

?>