void test1(bool cond)
{
  char* x;
  if (cond)
    x = "tainted";
  else
    x = "safe";
  /* ruleid: taint-maturity */
  sink(x);
}

void test2(bool cond)
{
  char* x;
  x = "tainted";
  if (cond)
    x = sanitize(x);
  /* ruleid: taint-maturity */
  sink(x);
  x = sanitize(x);
  /* OK: taint-maturity */
  sink(x);
}

void test3()
{
  char* x, y;
  x = "tainted";
  if (cond)
  {
    /* ruleid: taint-maturity */
    sink(x);
    y = sanitize(x);
  }
  else
    y = sanitize(x);
  /* ruleid: taint-maturity */
  sink(x);
  /* OK: taint-maturity */
  sink(y);
}

