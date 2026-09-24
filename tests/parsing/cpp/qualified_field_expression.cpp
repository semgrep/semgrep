template <typename T>
void call(T object) {
  object.template method<int>();
  object.template operator()<int>();
}

struct Base {
  void method();
  int field;
};

struct Derived : Base {};

void qualified_call(Derived object) {
  object.Base::method();
  int value = object.Base::field;
  if (object.Base::field < 0 || value >= 1) {
    value = 0;
  }
}
