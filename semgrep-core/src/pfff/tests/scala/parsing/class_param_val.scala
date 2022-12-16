class RelPath private[os](segments0: Array[String], val ups: Int)
  extends FilePath with BasePathImpl with SegmentedPath {
  def last = segments.last
  val segments: IndexedSeq[String] = segments0
  type ThisType = RelPath
  require(ups >= 0)
  protected[this] def make(p: Seq[String], ups: Int) = {
    new RelPath(p.toArray[String], ups + this.ups)
  }
}
