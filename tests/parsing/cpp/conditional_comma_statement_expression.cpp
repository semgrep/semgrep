int expressions(bool b, int x) {
  int result = b ? x++, x : 0;
  int statement = ({ int value = x; value; });
  return __extension__ (result + statement);
}
