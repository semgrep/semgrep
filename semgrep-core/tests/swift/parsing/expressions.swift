// -----------------------------------------------------------------------------
// Identifiers:
// -----------------------------------------------------------------------------
foo;

// -----------------------------------------------------------------------------
// Unary expressions:
// -----------------------------------------------------------------------------

// Postfix expressions:
5++;
5--;
bar!;

// Call expressions:
foo(bar, baz);
foo();
isthis[obj, c];
bar[];

named(foo: 3);
named(foo: 3, bar: 4);
// An argument named "async" is special-cased in the parser
named(async: 3);
// Not actually a function call, but parsed as one
named(bar: baz:);
// TODO figure out the type modifier part of a value argument

closure { x in x };

doubleclosure { x in x } more: { y in y };

// TODO tree-sitter-swift parses this incorrectly. The closure should be
// considered the final argument to the function call as above, but instead this
// is parsed as two separate call expressions where the lambda is the argument
// to the return value of the single-argument function call.
mixedargs(foo: 3) { x in x };
