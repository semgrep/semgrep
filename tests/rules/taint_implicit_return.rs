
// ruleid: taint-implicit-return
fn foo(s : String) -> String{
  s
}

// ruleid: taint-implicit-return
fn foo(s : String) -> String{
  let some_var = s;
  some_var
}

// ruleid: taint-implicit-return
fn foo(s : String) -> String{
  let some_var = s;
  s
}

// ruleid: taint-implicit-return
fn foo(s : String) -> String{
  let some_var = "s";
  s
}
