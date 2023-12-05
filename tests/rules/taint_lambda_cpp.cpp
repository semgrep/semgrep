extern void source(int *p);
extern void sink(int *p);

void correct(int *p) {
    source(p);
    // ruleid: source-sink
    sink(p);
}

void incorrect(int *p) {
    // There is no flow from `source` to `sink`
    auto f1 = [&p]() {
        source(p);
    };
    auto f2 = [&p]() {
        // ok: source-sink
        sink(p);
    };
}

void correct_in_lambda() {
    // Within a lambda, there is a flow from `source` to `sink`
    auto f = []() {
        int *p = new int;
        source(p);
        // ruleid: source-sink
        sink(p);
    };
}

void todo_only_sink_in_lambda(int *p) {
    // We assume that an unused lambda is called at the end of the parent
    // function.
    source(p);
    auto f2 = [&p]() {
        // ruleid: source-sink
        sink(p);
    };
}
