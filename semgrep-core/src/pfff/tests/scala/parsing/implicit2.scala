trait WritableLowPri {
   implicit def WritableTraversable[M[_], T](a: M[T])
                                         (implicit f: T => geny.Writable,
                                          g: M[T] => TraversableOnce[T]): Source = {
    val traversable = g(a)
    val f0 = f
    new Source {
      def getHandle() = Left(
        new geny.Writable{
          def writeBytesTo(out: java.io.OutputStream) = {
            for(x <- traversable) f0(x).writeBytesTo(out)
          }
        }
      )
    }
   }

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
