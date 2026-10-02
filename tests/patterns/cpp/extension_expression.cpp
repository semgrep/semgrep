int expressions(int x) {
  //ERROR: match
  int value = __extension__ (x + 2);
  int other = __extension__ (x - 2);
  return value + other;
}
