// from zio
trait GenPoly {
  type T
  val genT: Gen[Random with Sized, T]
}
