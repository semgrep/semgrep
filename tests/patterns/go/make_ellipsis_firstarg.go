func f() a{
  //OK:
  ch := make(chan string)
  //ERROR:
  ch := make(chan string, 1)
  //ERROR:
  ch := make(chan string, 1, 2)
  //OK:
  x := make(int)
}
