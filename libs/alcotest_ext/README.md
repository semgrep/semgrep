# Alcotest_ext (temporary name)

This is an extension of the
[Alcotest library](https://github.com/mirage/alcotest).
At the time of writing, it uses Alcotest for running tests and adds a layer
that stores past test results, including:

- comparing test outcome with expectation, allowing tests to be expected to
  fail;
- comparing captured output with expected output;
- approving new output;
- listing test status without running them;
- nested test suites;
- tags;
- more ways to select tests.

Like with Alcotest, a test executable is generated from a list of tests
written in OCaml. The core subcommands are:

- `run`
- `status`
- `approve`
