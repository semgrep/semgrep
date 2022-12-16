object RelPath {
  def apply[T: PathConvertible](f0: T): RelPath = {
    val f = implicitly[PathConvertible[T]].apply(f0)

    require(!f.isAbsolute, s"$f is not a relative path")

    val segments = BasePath.chunkify(f.normalize())
    val (ups, rest) = segments.partition(_ == "..")
    new RelPath(rest, ups.length)
  }
}
