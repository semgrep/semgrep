function foo() {
    sink("safe");
}

function bad_for_sym_prop(t) {
    for(;t=f(t);)
        t=g(t);
    return t;
}
