struct foo {
  int fld1;
};

void main(void) {
  struct foo a;
  struct foo *b;
  a.fld1 = 2;
  b->fld1 = 3;
}
