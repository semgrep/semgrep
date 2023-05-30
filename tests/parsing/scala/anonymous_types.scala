trait Test { self =>
  def foo[T](x: A[? >: T] => T2): Int =
  { (x: A[? >: T]) => 3 
  }
}
