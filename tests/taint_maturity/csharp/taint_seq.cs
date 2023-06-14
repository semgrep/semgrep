public class Test
{
  public void test1()
  {
    //ruleid: taint-maturity
    sink("tainted");
  }

  public void test2()
  {
    string a, x, y, z;
    x = "safe";
    a = x;
    x = "tainted";
    y = x;
    z = y;
    //ruleid: taint-maturity
    sink(z);
    //OK: taint-maturity
    sink(a);
    //OK: taint-maturity
    safe(z);
  }

  public void test3()
  {
    //ok: taint-maturity
    sink(sanitize("tainted"));
  }

  public void test4()
  {
    string x;
    x = "tainted";
    x = sanitize(x);
    //ok: taint-maturity
    sink(x);
  }
}
