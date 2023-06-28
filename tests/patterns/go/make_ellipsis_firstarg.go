func f() a{
  //OK:
  ch := make(chan string)
  //ERROR:
  ch := make(chan string, 1)  // Missed by $CHANNEL := make(..., $FOO) and $CHANNEL := make($FOO, $VAR)
  //ERROR:
  ch := make(chan string, 1, 2)
  //OK:
  x := make(int)
}
