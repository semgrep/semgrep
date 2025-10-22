number = 42
val result = number match {
  // MATCH:
  case x @ 42 => "MATCH"
  // OK:
  case _ => "OK"
}
