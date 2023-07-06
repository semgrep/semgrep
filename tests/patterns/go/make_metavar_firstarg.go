func f() {
  //OK:
  y := make(int)
  //ERROR:
  x := make(int,150)
  //OK:
  z := make(int,150,150)
}
