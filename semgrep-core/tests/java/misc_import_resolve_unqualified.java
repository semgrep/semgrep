import Foo.Bar;

class Foo {

  @Misc
  @Foo.Bar(method="foobar")
  public int foo(int x) { 
    return x;
  }

  @Misc
  @Bar(method="foobar")
  public int foo(int x) { 
    return x;
  }
}
