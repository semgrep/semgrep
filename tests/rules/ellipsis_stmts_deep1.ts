
foo();

obj.met(async () => {
  something();
  // ruleid: test
  x = baz();
});

bar();

// ok: test
y = baz();
