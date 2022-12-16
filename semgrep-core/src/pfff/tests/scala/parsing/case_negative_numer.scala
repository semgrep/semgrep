object BasePath {
  def checkSegment(s: String) = {
    s.indexOf('/') match{
      case -1 => // do nothing
      case c => fail(
        s"[/] is not a valid character to appear in a path segment. " +
          "If you want to parse an absolute or relative path that may have " +
          "multiple segments, e.g. path-strings coming from external sources " +
          considerStr
      )

    }
  }
}
