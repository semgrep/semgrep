if (random()) {
  //ruleid: test
  sink(source);
}
else {
  throw "error";
  //OK:
  sink(source); // unreachable
}
