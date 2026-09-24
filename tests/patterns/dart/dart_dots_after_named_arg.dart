void main() {
  // ERROR:
  Widget(child: c1, key: k1);

  // ERROR:
  Widget(child: c2, key: k2, label: l2);

  // pattern `Widget(child: $C, ...)` ALSO matches a sole named arg
  // (the trailing `...` accepts an empty arg list):
  // ERROR:
  Widget(child: c3);

  // negative: a different named arg shouldn't match
  Widget(label: l4);

  // negative: different constructor
  Other(child: c5);
}
