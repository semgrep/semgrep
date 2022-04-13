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

// TODO this leads the parser into an infinite loop in our CI environment. I
// (nmote) have replicated it locally using the docker container that the
// build-tests GitHub action uses, but it does not reproduce on my machine
// otherwise. I verified by stepping through with gdb that the parser is indeed
// in an infinite loop in scanner.c. Here's a backtrace from attaching to
// semgrep-core while it's in the loop:
// https://gist.github.com/nmote/0bddd68b30f925e71a60086eeff9fe23
//
// Reproduced using the command:
//
// $ semgrep-core -lang swift -dump_tree_sitter_cst expressions.swift
//
// doubleclosure { x in x } more: { y in y };

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
