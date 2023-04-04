void test1()
{
  /* ruleid: taint-maturity */
  sink("tainted");
}

void test2()
{
  char* a, x, y, z;
  x = "safe";
  a = x;
  x = "tainted";
  y = x;
  z = y;
  /* ruleid: taint-maturity */
  sink(z);
  /* OK: taint-maturity */
  sink(a);
  /* OK: taint-maturity */
  safe(z);
}

void test3()
{
  /* OK: taint-maturity */
  sink(sanitize("tainted"));
}

void test4()
{
  char* x;
  x = "tainted";
  x = sanitize(x);
  /* OK: taint-maturity */
  sink(x);
}