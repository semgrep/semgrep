class C 
{
    public C(int a, int b)
    { }
}

class Test {
    public void test() {
      //ERROR: match
      var c = new C(1, 2);
    }
}
