// no match cause they aren't the same names
match f() {
  Abc(_) => {}
  Def(_) => {}
}

match f() {
  Err(_) => {}
}

match f() {
  Ok(_) => {}
}

// MATCH:
match f() {
  Ok(_) => {}
  Err(_) => {}
}

// MATCH:
match f() {
  Err(_) => {}
  Ok(_) => {}
}
