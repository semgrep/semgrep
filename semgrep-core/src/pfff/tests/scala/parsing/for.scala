trait WritableLowPri {
  implicit def WritableGenerator[T](a: geny.Generator[T])(implicit f: T => geny.Writable): Source ={
    val f0 = f
    new Source {
      def getHandle() = Left(
        new geny.Writable{
          def writeBytesTo(out: java.io.OutputStream) = {
            for(x <- a) f0(x).writeBytesTo(out)
          }
        }
      )
    }
  }
}
