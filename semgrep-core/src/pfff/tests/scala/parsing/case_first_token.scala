case class Shellable(value: Seq[String])
object Shellable{
  /*
  implicit def StringShellable(s: String): Shellable = Shellable(Seq(s))

  implicit def SymbolShellable(s: Symbol): Shellable = Shellable(Seq(s.name))
  implicit def PathShellable(s: Path): Shellable = Shellable(Seq(s.toString))
  implicit def RelPathShellable(s: RelPath): Shellable = Shellable(Seq(s.toString))
   */
  // context bound here, T: Numeric
  implicit def NumericShellable[T: Numeric](s: T): Shellable = Shellable(Seq(s.toString))
/*
  implicit def IterableShellable[T](s: Iterable[T])(implicit f: T => Shellable): Shellable =
    Shellable(s.toSeq.flatMap(f(_).value))

  implicit def ArrayShellable[T](s: Array[T])(implicit f: T => Shellable): Shellable =
    Shellable(s.flatMap(f(_).value))
 */
}
