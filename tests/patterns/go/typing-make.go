func f() {
  x := make(a)
  y := new(a)
  //ERROR:
  bad := x.bad()
  //OK:
  ok := y.ok()
  z := &x
  //OK:
  sleep(z,z,z,z,z)
  //OK:
  z.sleep()
  //ERROR:
  return x.bad()
}
