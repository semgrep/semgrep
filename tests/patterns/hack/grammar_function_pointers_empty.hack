<?hh
// ERROR: match
consume(foo<>, $value);
consume(foo(), $value);
consume(foo<int>, $value);
