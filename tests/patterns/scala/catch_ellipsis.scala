
val x = 
  try {
    2
  } catch {
    case ex: Exception => 3
  }

val x = 
  try {
    2
  } catch 4 