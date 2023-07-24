func f() {
  //ERROR:
  x := make(int,1,2)
  //ERROR:
  x := make(int,2)
  x := make(int)
}
