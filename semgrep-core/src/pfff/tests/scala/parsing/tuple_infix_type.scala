object Foo {
  implicit def curryUncurryAdjunction[S]: (S, *) -| (S => *) =
      new Adjunction[(S, *), (S => *)] {
        override def leftAdjunct[A, B](a: => A)(f: ((S, A)) => B): S => B = s => f((s, a))
        override def rightAdjunct[A, B](a: (S, A))(f: A => S => B): B = f(a._2)(a._1)
      }
}
