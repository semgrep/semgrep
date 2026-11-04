package statements

func bodies(value interface{}, ch chan int) {
  // ERROR: match
  first(); middle(); second()
  switch value {
  case 1:
    // ERROR: match
    first(); second()
  default: first(); last()
  }
  switch value.(type) {
  case int:
    // ERROR: match
    first(); middle(); second()
  default: first(); last()
  }
  select {
  case <-ch:
    // ERROR: match
    first(); second()
  default: first(); last()
  }
}
