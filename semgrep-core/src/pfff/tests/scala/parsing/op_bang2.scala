object Foo {

  def binaryop[_: P] = P(
    StringIn(
      "<<", ">>", "<=", ">=", "in", "==", "!=", "&&", "||",
      "*", "/", "%", "+", "-", "<", ">", "&", "^", "|"
    )

  ).!

  def document[_: P]: P[(Expr, Map[String, Int])] = P( expr ~  Pass(P.current.misc.toMap.asInstanceOf[Map[String, Int]]) ~ End )

}
