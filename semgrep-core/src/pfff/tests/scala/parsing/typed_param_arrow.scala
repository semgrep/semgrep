object Foo {
  val relPathOrdering =
     Ordering.by((rp: RelPath) => (rp.ups, rp.segments.toIterable))

}
