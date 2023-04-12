// MATCH:
type t = K match {
  case String => Int;
  case Int => Int;
}