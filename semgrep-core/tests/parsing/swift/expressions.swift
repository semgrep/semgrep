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
{ @foo [ self, x, y = 3]
(self, name, realname : inout @escaping @autoclosure Int ) -> Int in
return 2
};

{ @foo [ self, x, y = 3]
self, name, realname : inout @escaping @autoclosure Int -> Int in
return 2
};

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

#selector(5);
#selector(getter: 4);
#selector(setter: 4);

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

// -----------------------------------------------------------------------------
// Ternary expressions:
// -----------------------------------------------------------------------------

true ? 5 : 7;

// Handled separately in the grammar
true ? 5 : foo();

// -----------------------------------------------------------------------------
// Primary expressions:
// -----------------------------------------------------------------------------

// Tuple expressions

(1, 2);
(thing: 1, other: 2);

// Basic literals

42;
4_2;

0x5f;
0X5F;

0o40;
0O21;

0b10;
0B10;

3.4;
3e5;
3E+54;
3e-2;

true;
false;

"foo\t\u{1F600}\(5 + 3)";
""" "asdf
"
"""

// TODO raw string literal

nil;

// Lambda literals

// TODO support captures
// TODO figure out if { x in } is legal
({ x });
({ x in x });
({});

// Special literals
#file;
#fileID;
#filePath;
#line;
#column;
#function;
#dsohandle;
#colorLiteral(x : 5, y : 2);
#fileLiteral(x : 5);
#imageLiteral(x : 5);
#keyPath(2 + 3);


// TODO Playground literals

// Array literals

[1, 2, 3];
[1,];
// Empty collections require an explicit type
([] as [Int]);

// Dictionary literals

[1: 2, 3: 4];
[1: 2,];
// Empty collections require an explicit type
([:] as [Int : Int]);

// Self expressions

self

// Super expressions

super

// Try expressions

try (5 + foo());
try! true && false;
try? bar();
try x ? y : z;

// Await expressions

await 5;
await foo();
await x ? y : z;

// TODO Referenceable operators
// TODO Key path expressions
// TODO Key path string expressions

// Unbounded range expressions

(...);

// -----------------------------------------------------------------------------
// Assignment:
// -----------------------------------------------------------------------------

foo += 5;
foo.bar -= 5;
foo() *= 5;
(foo, bar) /= 5;
self %= 5;
foo = 5;

// Immediate question mark ?

foo?.bar;

// aysnc ?

async

// Operators
++;
--;
1 < 2;
1 > 2;
1 <= 2;
1 >= 2;
1 + 2;
1 * 2;
1 != 2;
1 !== 2;
1 === 2;
1 == 2;
!2;
~2;

// Declarations with operator names
func +() {}
func *() {}
func ==() {}
func /() {}
func >>=() {}