class GlobInterpolator(sc: StringContext) {
  def g(parts: Any*) =
     new StringContext(sc.parts:_*).s(parts:_*)
  def g =
    new GlobInterpolator.Interped(sc.parts)
}
