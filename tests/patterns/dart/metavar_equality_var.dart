// Polyglot pattern:
//   $V = open();
//   close($V);

dynamic myFile;

void test() {
  // ERROR:
  myFile = open();
  close(myFile);
}
