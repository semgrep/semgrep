void main() {
  // ERROR:
  Widget(0, child: c1);

  // ERROR:
  Widget(0, 1, child: c2);

  // pattern `Widget(..., child: $C)` ALSO matches when preceding args are named
  // (the leading `...` accepts a sequence of named args before `child:`):
  // ERROR:
  Widget(key: k, child: c3);
  // ERROR:
  Widget(key: k, label: l, child: c4);

  // pattern also matches when there are no preceding args at all
  // (the leading `...` accepts an empty arg list):
  // ERROR:
  Widget(child: c5);

  // negative: different constructor / unrelated call
  Other(child: c6);

  // negative: no `child:` named arg at all
  Widget(0, 1);
}
