int main() {
  #ERROR:
  int x = 0;
  #ERROR:
  x;
  #ERROR:
  x + x;
  #OK
  x = 1
  #OK
  return x;
}

