object bar:
  private def foo(x: Int) =
    x match
      case p if p < 15 => "a"
      case p if p < 30 => "b"
      case p if p < 45 => "c"
      case _           => "d"
