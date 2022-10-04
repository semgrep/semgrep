import Foo.Bar;

class Foo {

  //ERROR: match
  @Misc
  @Foo.Bar(method="foobar")
  public int foo(int x) { 
    return x;
  }

  //ERROR: match
  @Misc
  @Bar(method="foobar2")
  public int foo2(int x) { 
    return x;
  }
}
