trait Comments { this: Parser =>

  var lastBreaks = mutable.MutableList[Int](0)

  implicit def offset2Loc(i: Int): Loc = {
    val idx = lastBreaks.lastIndexWhere(brk => brk <= i)
    Loc(idx + 1, i - lastBreaks(idx) + 1)
  }
}
