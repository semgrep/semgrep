// ERROR: match
fn foo(self, foo : t) { }

// ERROR: match
fn foo(self) { }

// ERROR: match
fn foo(foo : t) { }

// ERROR: match
fn foo(i32, foo : t) { }

// ERROR: match
fn foo(i32) { }

// OK:
fn foo() { }
