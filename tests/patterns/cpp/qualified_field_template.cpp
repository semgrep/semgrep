struct Base {
  void method();
};

struct Derived : Base {};

void calls(Derived object) {
  //ERROR: match
  object.Base::method();
  object.method();
  object.Base::other();
}
