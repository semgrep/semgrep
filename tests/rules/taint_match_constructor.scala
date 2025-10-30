package test

object MatchTaintConstructor {
  def source(): String = "tainted"
  def sink(x: String): Unit = {}

  def test1(opt: Option[String]): Unit = {
    val tainted = source()
    val wrapped = Some(tainted)

    wrapped match {
      // todoproruleid: taint-match-constructor
      case Some(x) => sink(x)
      case None => ()
    }
  }
}
