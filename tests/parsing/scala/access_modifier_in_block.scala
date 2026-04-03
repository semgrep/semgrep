trait QuestionMarkBindVariables { self: SqlIdiom =>

  override def liftingPlaceholder: String = s
}
