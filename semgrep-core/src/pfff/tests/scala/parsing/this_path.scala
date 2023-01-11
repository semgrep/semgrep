trait SegmentedPath extends BasePath{
  protected[this] def make(p: Seq[String], ups: Int): ThisType
  /**
    * The individual path segments of this path.
    */
  def segments: IndexedSeq[String]

  def /(chunk: PathChunk) = make(
    segments.dropRight(chunk.ups) ++ chunk.segments,
    math.max(chunk.ups - segments.length, 0)
  )

  def endsWith(target: RelPath): Boolean = {
     this == target || (target.ups == 0 && this.segments.endsWith(target.segments))
  }
}
