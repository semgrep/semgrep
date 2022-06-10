class C {
    val x = foo match {
    case Foo(x : A) => e1
    case Bar(y : B) => e2
    }
}