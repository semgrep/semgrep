struct Lock {
  int lock;
};

struct Bar {
  // kencc extension
  Lock;
  int v;

  struct X {
  } v;
};
