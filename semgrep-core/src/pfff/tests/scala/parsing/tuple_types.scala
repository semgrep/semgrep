object Foo {
  def attrs(path: Path,
            includeTarget: Boolean = false): IndexedSeq[(Path, os.StatInfo)] = {
    stream.attrs(path, skip, preOrder, followLinks, maxDepth, includeTarget)
      .toArray[(Path, os.StatInfo)]
  }
}
