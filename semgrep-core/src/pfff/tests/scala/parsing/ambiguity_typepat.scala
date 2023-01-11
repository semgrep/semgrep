class Foo {
  override def hashCode = segments.hashCode() + ups.hashCode()
  override def equals(o: Any): Boolean = o match {
    // in a pattern, you can't use functionType type because there
    // is an ambiguity with the case block.
    case p: RelPath => segments == p.segments && p.ups == ups
    case p: SubPath => segments == p.segments && ups == 0
    case _ => false
  }
}
