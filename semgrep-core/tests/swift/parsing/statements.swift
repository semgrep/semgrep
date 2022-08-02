// -----------------------------------------------------------------------------
// Global Declarations
// -----------------------------------------------------------------------------

// Import

// TODO include modifiers
import foo;
import foo.bar;
import typealias foo.bar;
import struct foo;
import class foo;
import enum foo;
import protocol foo;
import let foo;
import var foo;
import func foo;

// Constant

// TODO this is quite incomplete, add more cases as needed

let foo = 3;
let (foo) = 3;
let foo: Int = 4;
let (foo, bar) = (1, 2);
// TODO
// let (foo: baz, bar) = (1, 2);
let foo: Int = 1, bar = 2;

// Variable

// TODO this is quite incomplete, add more cases as needed

var foo = 1;
var bar: Int = 2;
var baz: Int;

// TODO computed variables
// var computed: Int {
//     get { 4 }
//     set { }
// }

// Typealias

// TODO include modifiers
typealias foo = bar;
typealias foo<baz> = bar;
typealias foo<baz: Int> = bar;

// Function

// TODO include modifiers
func foo() { }
func foo() -> Int { return 5; }
func foo(x: Int, y: Int) { }
func foo(_: Int) { }
func foo(_ x: Int) { }
func foo(x y: Int) { }
// TODO
// func foo(x: inout Int) { }
func foo(x: Int = 5) { }
func foo(x: Int...) { }
// TODO
// func foo() throws { }
// func foo() throws -> Int { }
// func foo(f: () throws -> void) rethrows { }
// func foo() async { }
// func foo() async -> Int { return 5; }
// func foo() async throws { }

// Class etc.

class foo {
    // According to the grammar, and even the Swift spec, any declaration is
    // allowed here. But Swift rejects imports after parsing, so they are
    // omitted here.
    let foo = 1;
    var bar = 2;
    typealias asdf = Int;
    func f() { }
    init() { }
    init(foo: Int, bar: Int) { }
    enum baz { }
    struct stru { }
    class cla { }
    protocol prot { }
    deinit { }
    // TODO others
}
class bar: foo { }
class bar: foo, baz, asdf { }
class foo<bar, baz> { }
class foo<bar: baz> { }
// TODO
// class foo<bar> where bar: baz { }
// class foo<bar> where bar: baz, bar: asdf { }

// TODO test other varieties
struct foo { }

// TODO test other varieties
extension bar { }

// TODO test other varieties
enum baz { }

// For-in loops
for x in collection {
}
for await x in collection {
}
for try await x in collection {
}
for try? await x in collection {
}
for try! await x in collection {
}
for x in collection where true == true {
}

// While loops
while true {}
while case x = true {}
while async let x = true {}
while async var x : true = true {}

// Repeat-while loops
repeat {} while true
repeat {} while case x = true
repeat {} while async let x = 1

// Do-statements
do {
  var x = 2
} catch foo {}

do {
  var x = 2
} catch {}

do {
  var x = 2
} catch foo {
} catch bar where true == true {
} catch {
}

// If-statements
if true {}
if true {} else {}
if async let x = true {} else {}
if case x = true {} else if case x = false {} else {}

// Protocol

protocol foo {
    // The grammar allows bodies for functions/initializers/deinitializers but
    // Swift actually does not.
    //
    // The grammar allows deinits here but Swift does not.
    func foo()
    init()
    var foo: Int { get }
    var foo: Int { get set }
    var foo: Int { set get }
    // TODO other options
}

protocol foo<T> { }
protocol foo: bar { }
protocol foo: bar, baz { }
// TODO
// protocol foo<T> where T: bar { }

foo: if 1 < 150 {
}

bar: for x in collection {
}