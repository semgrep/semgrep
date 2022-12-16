implicit class ArrayPathChunk[T](a: Array[T])(implicit f: T => PathChunk) extends PathChunk {
    val inner = SeqPathChunk(a)(f)
    def segments = inner.segments
    def ups = inner.ups

    override def toString() = inner.toString
  }
