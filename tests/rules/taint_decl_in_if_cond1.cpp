void test1() {
    void *data = get_data();

    source(data);
    // assume that any call with `&data` will
    // sanitize `data`
    maybe_realloc_data(&data);
    // ok: source-sink-while
    sink(data);
}

void test2() {
    void *data = get_data();

    // BUG: Declaration in `while` loop does not sanitize
    while(auto ret = maybe_realloc_data(&data)) {
        // ok: source-sink-while
        sink(data);
        source(data);
    }
}

void test3() {
    void *data = get_data();

    auto ret;

    // Assignment in `while` loop /does/ sanitize
    while(ret = maybe_realloc_data(&data)) {
        // ok: source-sink-while
        sink(data);
        source(data);
    }
}
