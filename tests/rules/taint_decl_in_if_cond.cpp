void test1 () {
    if (const char *tainted_or_null = source("PATH"))
    {
        // ruleid: if_var_decl_init_maybe_null
        sink(tainted_or_null);
    }
}

void test2 () {
    const char *tainted_or_null = source("PATH");
    if (tainted_or_null) {
        // ruleid: if_var_decl_init_maybe_null
        sink(tainted_or_null);
    }
}
