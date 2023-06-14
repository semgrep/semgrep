val x = 
  // ERROR:
  try {
    2
  } catch {
    case ex: Exception => 3
  }

val x = 
  // ERROR:
  try {
    2
  } catch 4 