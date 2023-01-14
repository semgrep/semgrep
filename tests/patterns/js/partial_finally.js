function test() {
  try {
    return 0;
  } finally {
    //ERROR: match
    return 1;
  }
}
