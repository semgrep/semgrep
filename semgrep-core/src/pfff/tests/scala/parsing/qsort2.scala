object Foo {
  val qsort: List[Int] => List[Int] = {
  case Nil => Nil
  case pivot :: tail =>
    val (smaller, rest) = tail.partition(_ < pivot)
    qsort(smaller) ::: pivot :: qsort(rest)
}
}
