# ruleid:test
sink(foo(input()));

# ruleid:test
sink(bar(foo(input())));

# ruleid:test
sink(foo(bar(input())));

# ok:test
sink(bar(baz(foo())));

# ok:test
sink(bar(input()));
