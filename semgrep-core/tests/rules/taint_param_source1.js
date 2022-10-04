function foo({user_input}) {
  x = user_input;
  //ruleid:tainting
  sink(x);
}

function bar({user_input}) {
  user_input = 1;
  //ok:tainting
  sink(user_input);
}
