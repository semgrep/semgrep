
val x = 
  try 2
  catch case _ => 4

val x = 
  try 
    3
  catch
    case _ =>
      // we need to properly emit an indentRegion here, because we're
      // not just parsing a block, we're parsing an expr 
      // (which results in a <<< block >>>, actually)
      var x = 2

private val y = 3