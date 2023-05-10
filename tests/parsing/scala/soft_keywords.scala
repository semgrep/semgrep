
val x = foo { export =>
  // here, we scan one token ahead and see "export" is not followed by
  // an ident... so we treat it as an expr
  export match {
    case _ => 2
  }
}

val y = enum