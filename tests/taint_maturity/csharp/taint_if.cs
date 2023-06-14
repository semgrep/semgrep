public class Test
{
  public void test1(bool cond)
  {
    string x;
    if (cond)
      x = "tainted";
    else
      x = "safe";
    //ruleid: taint-maturity
    sink(x);
  }

  public void test2(bool cond)
  {
    string x;
    x = "tainted";
    if (cond)
      x = sanitize(x);
    //ruleid: taint-maturity
    sink(x);
    x = sanitize(x);
    //OK: taint-maturity
    sink(x);
  }

  public void test3()
  {
    string x, y;
    x = "tainted";
    if (cond)
    {
      //ruleid: taint-maturity
      sink(x);
      y = sanitize(x);
    }
    else
      y = sanitize(x);
    //ruleid: taint-maturity
    sink(x);
    //OK: taint-maturity
    sink(y);
  }
}
