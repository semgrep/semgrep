function foo({user_input} : Foo) {
  x = user_input;
  //ruleid:tainting
  sink(x);
}

function bar({user_input} : Bar) {
  user_input = 1;
  //ok:tainting
  sink(user_input);
}
