// MATCH:
foo.bar;
// MATCH:
foo.bar();
// MATCH:
foo.baz.bar;
// MATCH:
foo.baz().bar;
// MATCH:
foo.a.b.c.bar;
// MATCH:
foo.a.b().c.bar;
// MATCH:
foo.a().b().c().bar;
