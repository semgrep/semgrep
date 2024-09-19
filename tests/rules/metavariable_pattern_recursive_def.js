function test() {
    
    a == a;

	a == b;

    1 == 1;

    // TODO I don't understand this match but I don't know if it's worth investigating
    //ruleid: metavariable-pattern-keep-env
    f() == f();

    f() == g();
}
