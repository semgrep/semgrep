// ok because of how generic mode works
foo(1234);

// ruleid:
foo(2);

// ruleid:
foo(1, 2, 3, 4);

foo(1, 3, 4);

// ruleid:
foo('2');
