object TestQueryParser{
  /*
  def apply(input: String) = {
    parse(input).fold(e => throw new QueryParseError(input, e), x => x)
  }
  def parse(input: String) = new TestQueryParser(input).curlies(0) match{
    case Right((v, i)) =>
      if (i == input.length) Right(collapse(v))
      else Left(s"Expected end of input at index $i")
    case Left(e) => Left(e)
  }
  */
  /**
    * Combine common prefixes, converting
    *
    * {foo.bar,foo.baz}
    *
    * into
    *
    * foo.{bar,baz}
    */
  def collapse(input: TestQueryParser#Trees): TestQueryParser#Trees = {

    val mapping = mutable.Map.empty[String, Int]
    val ordered = mutable.Buffer.empty[List[Tree[String]]]
    for(subtree <- input){
      mapping.get(subtree.value) match{
        case None =>
          mapping(subtree.value) = ordered.length
          ordered.append(List(subtree))
        case Some(i) =>
          ordered(i) = subtree :: ordered(i)
      }
    }
    (for (grouping <- ordered) yield {
      Tree(grouping.head.value,
        collapse(grouping.reverse.flatMap(_.children)):_*
      )
    }).toSeq
  }
}
