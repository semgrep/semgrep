// https://github.com/returntocorp/semgrep/issues/3122

function foo(someVar, obj) {
  two = 2 // implicit declaration
  //ERROR:
  value = obj[two]
}
