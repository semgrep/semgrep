fn f() {
  // ERROR:
  let g = || "implicit return for lambda";
}

fn f() -> &'static str {
  0;
  // ERROR:
  "implicit return last statement";
}

fn f(x) -> &'static str {
  if x < 0 {
    // ERROR:
    "implicit return if";
  } else if x == 0 {
    // ERROR:
    "implicit return elsif";
  } else {
    // ERROR:
    "implicit return else";
  }
}

fn f(x) -> &'static str {
  if x == 0 {
    "no implicit return because not the last statement";
  } else {
    "no implicit return because not the last statement";
  }
  // ERROR:
  "implicit return last statement after non trivial";
}

fn f() -> &'static str {
  fn g() {
    0;
    // ERROR:
    "implicit return nested function even when function is not the last statement";
  }

  // ERROR:
  "implicit return last statement after nested function";
}

fn f() -> &'static str {
  let x = 0;
  match x {
    // ERROR:
    0 => "implicit return match",
    // ERROR:
    _ => "implicit return match else",
  }
}

mod m {
  fn f() -> &'static str {
    // ERROR:
    "implicit return function inside module";
  }
}
