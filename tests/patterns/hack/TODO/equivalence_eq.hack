// TODO: How is this different than metavar_equality_expr?

function foo() {
    $a = 1;
    $b = 2;
    //ERROR:
    if ($a + $b == $a + $b) {
        return 1;
    }

    if ($a+$b != $a+$b) {
        return 1;
    }

    if ($a+$b === $a+$b) {
        return 1;
    }

    if ($a+$b !== $a+$b) {
        return 1;
    }

    if(1 === 1) {
        return 1;
    }

    if(1 === 2) {
        return 1;
    }

    return 0;
}
