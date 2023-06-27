func goo() {
  //ERROR:
  x := make(map[string]int)
  //ERROR:
  x := make(map[string]int, 5)
  //ERROR:
  x := make(map[string]int, 0, 5)
  //OK:
  x := new(int)
}
