// Polyglot pattern:
//   $V = get();
//   ...
//   eval($V);
//
// Dart requires the assignment target to be declared, so `userData` is
// brought into scope as a top-level field first and the function body
// performs explicit assignments (not declarations) to match `$V = get()`.

dynamic userData;

void test() {
  // ERROR:
  userData = get();
  print("do stuff");
  foobar();
  eval(userData);
  foobar();
}
