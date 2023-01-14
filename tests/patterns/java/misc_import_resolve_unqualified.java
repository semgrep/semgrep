import Foo.Bar;

class Foo {

  @Misc
  @Foo.Bar(method="foobar")
  public int foo(int x) { 
    return x;
  }

  // We used to not match the code below and force the user to use
  // a fully qualified attribute like @Foo.Bar.Misc but I think
  // it's better to also allow direct match
  //ERROR: match
  @Misc
  @Bar(method="foobar")
  public int foo(int x) { 
    return x;
  }
}
