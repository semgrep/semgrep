void f()
{
  // ruleid: new-expr
  new int;
  // ruleid: new-expr
  new int(0);

  // ok: new-expr
  int {};
  // ok: new-expr
  int {0};
  // ok: new-expr
  int ();
  // ok: new-expr
  int (0);
}
