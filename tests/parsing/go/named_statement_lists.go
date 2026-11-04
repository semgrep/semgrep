package statements

func bodies(value interface{}, ch chan int) {
  first(); second()
  switch value { case 1: first(); second(); default: last(); done() }
  switch value.(type) { case int: first(); second(); default: last(); done() }
  select { case received := <-ch: use(received); second(); default: last(); done() }
}
