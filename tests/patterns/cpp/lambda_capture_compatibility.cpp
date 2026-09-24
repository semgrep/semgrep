void captures(int x) {
  //ERROR: match
  auto copied = [x]() { return x; };
  //ERROR: match
  auto referenced = [&x]() { return x; };
  auto negative = [x]() { consume(x); };
}
