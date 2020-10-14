
//ERROR: match
@SomeAnnot(1)
class Foo {

  //ERROR: match
  @SomeAnnot(2)
  void foo() {
  }
}
