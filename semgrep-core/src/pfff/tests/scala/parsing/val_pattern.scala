object GlobInterpolator{
  class Interped(parts: Seq[String]){
    def unapplySeq(s: String) = {
      val Seq(head, tail@_*) = parts.map(java.util.regex.Pattern.quote)

      val regex = head + tail.map("(.*)" + _).mkString
      regex.r.unapplySeq(s)
    }
  }
}
