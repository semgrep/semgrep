function test() {
  try {
    foo();
  } catch (e) {
    //ERROR: match
    return e;
  }
}
