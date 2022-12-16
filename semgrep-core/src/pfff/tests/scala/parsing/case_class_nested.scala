object PathError{
  /*
  type IAE = IllegalArgumentException
  private[this] def errorMsg(s: String, msg: String) =
    s"[$s] is not a valid path segment. $msg"
  */

  case class InvalidSegment(segment: String, msg: String) extends IAE(errorMsg(segment, msg))

  case object AbsolutePathOutsideRoot
    extends IAE("The path created has enough ..s that it would start outside the root directory")

  case class NoRelativePath(src: RelPath, base: RelPath)
    extends IAE(s"Can't relativize relative paths $src from $base")
}
