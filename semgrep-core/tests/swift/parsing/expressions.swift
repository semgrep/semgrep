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

// TODO this used to lead the parser into an infinite loop in CI. Test that it's
// correctly parsed, even though that issue has been mitigated.
doubleclosure { x in x } more: { y in y };

// TODO tree-sitter-swift parses this incorrectly. The closure should be
// considered the final argument to the function call as above, but instead this
// is parsed as two separate call expressions where the lambda is the argument
// to the return value of the single-argument function call.
mixedargs(foo: 3) { x in x };

// Constructor expressions:

// TODO The examples here that don't involve type parameters are parsed as call
// expressions rather than constructor expressions, due to ambiguity in the
// grammar. It might not be possible to disambiguate in the general case without
// resolving names, but we should make sure that these constructs are matched by
// patterns as users expect.

// array type
[Int]();
[Thing<Int>]();
// dict type
[Int: Int]();
[Int: Thing<Int>]();
// user type
UserType(bar);
UserType<Int>(bar);

[Int] { x in y };
[Thing<Int>] { x in y };

// Navigation expressions:

5.foo;
Int.foo;
// TODO This is parsed as two comparison expressions. That's most likely wrong.
Thing<Int>.foo;
Thing<Int, String>.foo;
// TODO This is parsed as a navigation expression where the first part is an
// expression rather than an array type. Figure out if that's the correct
// resolution to the ambiguity.
[Int].5;
[Thing<Int>].5;
// TODO This is parsed as a navigation expression where the first part is an
// expression rather than a dict type. Figure out if that's the correct
// resolution to the ambiguity.
[Int: Int].42;
[Int: Thing<Int>].42;

// Prefix expressions:

++5;
--5;
-5;
+5;
!5;
&5;
~5;
.foo;
.5;
// Custom operators
/!5;
/!<5;

// As expressions:

"5" as Int;
"5" as? Int;
"5" as! Int;

// Selector expressions:

// TODO handle selector expressions
// #selector(5);
// #selector(getter: 4);
// #selector(setter: 4);

// Open start range expressions:

...5;
..<5;

// Open end range expressions:

5...;

// -----------------------------------------------------------------------------
// Binary expressions:
// -----------------------------------------------------------------------------

// Multiplicative expressions:

5 * 5;
5 / 2;
5 % 2;

// Additive expressions:

5 + 2;
5 - 2;

// Range expressions:

1...5;
1..<5;

// Infix expressions:

5 /. 5;

// Nil coalescing expressions:

nil ?? nil;

// Check expressions:

5 is Int;

// Equality expressions:

5 != 5;
5 !== 5;
5 == 5;
5 === 5;

// Comparison expressions:

5 < 5;
5 > 5;
5 <= 5;
5 >= 5;

// Conjunction expressions:

true && false;

// Disjunction expressions:

true || false;

// Bitwise expression:

5 & 5;
5 | 5;
5 ^ 5;
5 << 1;
5 >> 1;
