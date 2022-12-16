object Format{
  /*
  case class FormatSpec(label: Option[String],
                        alternate: Boolean,
                        zeroPadded: Boolean,
                        leftAdjusted: Boolean,
                        blankBeforePositive: Boolean,
                        signCharacter: Boolean,
                        width: Option[Int],
                        precision: Option[Int],
                        conversion: Char)
  import fastparse._, NoWhitespace._
  def integer[_: P]           = P( CharIn("1-9") ~ CharsWhileIn("0-9", 0) | "0" )
  def label[_: P] = P( ("(" ~ CharsWhile(_ != ')').! ~ ")").? )
  def flags[_: P] = P( CharsWhileIn("#0\\- +", 0).! )
  def width[_: P] = P( (integer | "*").!.? )
  */
  def precision[_: P] = P( ("." ~/ integer.!).? )

  def conversion[_: P] = P( CharIn("diouxXeEfFgGcrsa%").! )
  def formatSpec[_: P] = P( label ~ flags ~ width ~ precision ~ CharIn("hlL").? ~ conversion ).map{
    case (label, flags, width, precision, conversion) =>
      FormatSpec(
        label,
        flags.contains('#'),
        flags.contains('0'),
        flags.contains('-'),
        flags.contains(' '),
        flags.contains('+'),
        width.map(_.toInt),
        precision.map(_.toInt),
        conversion.charAt(0)
      )
  }
}
