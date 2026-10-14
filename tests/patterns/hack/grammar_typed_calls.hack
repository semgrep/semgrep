<?hh
// ERROR: match
foo<int>(1);
foo<string>(1);
foo(1);
foo<int, string>(1);
