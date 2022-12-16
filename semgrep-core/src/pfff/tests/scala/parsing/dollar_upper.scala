object Foo {
  def visitExpr(expr: Expr)
               (implicit scope: ValScope, fileScope: FileScope): Val = try expr match{
    case $(offset) => foo
  }

}
