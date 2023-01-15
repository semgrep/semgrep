if (random()) {
  //ERROR:
  sink(source);
}
else {
  throw "error";
  //OK:
  sink(source); // unreachable
}
