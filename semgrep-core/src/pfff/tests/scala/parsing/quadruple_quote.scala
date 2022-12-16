object Foo {
  def foo =
    test("Invalid"){
      compileError("""root/"omg"/"wtf" < "omg"/"wtf"""")
    }
}
