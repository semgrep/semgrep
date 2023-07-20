// MATCH:
foo(() => {});
// MATCH:
foo(() => 5);
// MATCH:
foo(async () => 5);

// MATCH:
foo((x) => {});
// MATCH:
foo((x) => 5);
// MATCH:
foo(async (x) => 5);

// MATCH:
foo(x => {});
// MATCH:
foo(x => 5);
// MATCH:
foo(async x => 5);
