<?hh
// ERROR: match
consume(Example`1 + 2`, $value);
consume(Example`1 + 3`, $value);
consume(Different`1 + 2`, $value);
