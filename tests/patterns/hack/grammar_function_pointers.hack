<?hh
// ERROR: match
consume(foo<int>, $value);
consume(foo<string>, $value);
consume(foo<int>(), $value);
