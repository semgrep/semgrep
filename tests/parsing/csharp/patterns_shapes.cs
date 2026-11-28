// AST shapes of C# patterns, pinned by patterns_shapes.ast.expect.
class C {
  void M(object o, int[] xs, int n) {
    switch (o) {
      case Dog(var x) d: break;          // PatAs (PatTyped (PatTuple, Dog), d)
      case Rect { Width: 0 } r: break;   // PatAs (PatTyped (PatRecord), r)
    }
    if (o is { Width: 0, Height.Max: 1 }) { }   // PatRecord, dotted key
    if (xs is [1, ..] rest) { }                 // PatAs (PatList, rest)
    if (n is (> 0) m) { }                       // PatAs (relational, m)
    if (n is (1 or 2)) { }                      // PatDisj
    if (o is not null) { }                      // OtherPat not
    if (n is > 0 and < 10) { }                  // OtherPat and
    switch (n) { case -1: break; }              // CaseEqualExpr
  }
}
