<?php
// Tests for bool Xor via PHP's `xor` keyword (→ G.Xor).
// PHP is the only language in this test suite where binary bool XOR maps to
// G.Xor; Python/Java/Go `^` on booleans maps to G.BitXor instead.
//
// Each test folds bool literals through xor and uses SCCP to verify the result
// without needing a negative-boolean sink pattern.

function testXorTrueFalse() {
    // true xor false = true → if-branch is always live
    $a = true;
    $b = false;
    if ($a xor $b) {
        $result = "yes";
    } else {
        $result = "no";  // dead
    }
    //proruleid: test-sccp
    sink($result);
}

function testXorTrueTrue() {
    // true xor true = false → if-branch is always dead
    $a = true;
    $b = true;
    if ($a xor $b) {
        $result = "no";  // dead
    } else {
        $result = "yes";
    }
    //proruleid: test-sccp
    sink($result);
}

function testXorFalseFalse() {
    // false xor false = false → if-branch is always dead
    $a = false;
    $b = false;
    if ($a xor $b) {
        $result = "no";  // dead
    } else {
        $result = "yes";
    }
    //proruleid: test-sccp
    sink($result);
}

function testXorUnknown($a) {
    // $a is not constant; both branches are live
    $b = true;
    if ($a xor $b) {
        $result = "yes";
    } else {
        $result = "no";
    }
    // OK: not statically evaluable
    sink($result);
}
