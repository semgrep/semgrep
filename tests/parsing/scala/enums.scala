
enum Foo {
}

enum Foo: 
  val x = 2

enum Foo[+ T >: T1 <: T2] @foo private (x : Int) (y : String) {
}

enum Foo {
  @foo private case A
}

enum Foo {
  case A, B, C
}

enum Foo {
  case A[+T] @foo private (x : Int) (y : Int) extends Foo (3, 4, 5), Bar (3, 4, 5)
  case A[+T] @foo private (x : Int) (y : Int) extends Foo (3, 4, 5) with Bar (3, 4, 5)
  case A, B, C
}