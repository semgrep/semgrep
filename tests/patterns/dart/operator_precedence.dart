void main() {
  int a = 1, b = 2, c = 3;

  //ERROR: match
  print(a + b * c);

  //OK:
  print((a + b) * c);
}
