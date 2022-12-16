  object stream extends Function1[ReadablePath, geny.Readable]{
    def apply(p: ReadablePath): geny.Readable = new geny.Readable{
       def readBytesThrough[T](f: java.io.InputStream => T): T = {
        val is = p.getInputStream
        try f(is) finally is.close()
      }
    }
  }
