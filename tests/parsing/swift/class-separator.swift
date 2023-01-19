// Regression test for
// https://github.com/returntocorp/ocaml-tree-sitter-core/issues/36

class Foo {
    var foo: Bool
    /* foo */ let bar = 1

    init() {
        self.foo = true
    }
}

