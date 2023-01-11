class Foo {
  Foo() { }
  void foo() { }
};

class Foo2 {
public:
  Foo2() { }
  void foo() { }
};


class Foo3 : public Foo {
  Foo3() { }
  void foo() { }
};

class Foo4: public Foo3 {
public:
 Foo4(const vector<string>& filenames, bool generatingBias);
};
