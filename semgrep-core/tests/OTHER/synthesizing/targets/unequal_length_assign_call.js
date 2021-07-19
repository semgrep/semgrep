// Example of targets where a line in the middle should be ignored.
// Desired pattern would be:
// $X = req.query.foo;
// ...
// exec($X);

// Target 1
x = req.query.foo;
exec(x);

// Target 2
y = req.query.foo();
print(y);
exec(y);
