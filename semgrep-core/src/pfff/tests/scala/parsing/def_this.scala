class DotNode(val id: String, val label: String, val additionalParams: Map[String, String]) extends DotElement {

  def this(label: String, additionalParams: Map[String, String] = Map()) =
    this("n" + IDGenerator.getNewId, label, additionalParams)

  def this() = this("")

  def equals(other: DotNode): Boolean = toDotString.equals(other.toDotString)

  override def toString: String = toDotString

  def toDotString: String =
    id + "[label=\"" + Output.escape(label) + "\"" +
      additionalParams.map(p => s"${p._1} = ${p._2}").mkString(",") + "]"

}
