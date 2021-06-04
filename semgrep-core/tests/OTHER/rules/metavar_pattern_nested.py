# ruleid:test-mvp-nested
foo(bar(a + b), 2, 3)
# OK:test-mvp-nested
foo(a + b, 2, 3)
# OK:test-mvp-nested
bar(a + b)
