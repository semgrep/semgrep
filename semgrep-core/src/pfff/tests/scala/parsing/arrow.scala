object Foo {
  def apply(path: Path,
            skip: Path => Boolean = _ => false,
            includeTarget: Boolean = false): IndexedSeq[Path] = {
    stream(path, skip, preOrder, followLinks, maxDepth, includeTarget).toArray[Path]
  }
}
