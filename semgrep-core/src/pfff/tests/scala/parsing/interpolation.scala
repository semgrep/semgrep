object Foo {
  def pointsTo(): Map[ADeclaration, Set[AstNode]] = {
    val solution = solver.solution()
    val unifications = solver.unifications()
    log.info(s"Solution: \n${solution.mkString(",\n")}")
    log.info(s"Sets: \n${unifications.values.map { s =>
      s"{ ${s.mkString(",")} }"
    }.mkString(", ")}")
  }
}
