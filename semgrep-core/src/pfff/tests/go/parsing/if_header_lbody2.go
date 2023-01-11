package Foo

func test1() {
  if v, ok := interface{}(x).(interface{ Validate() error }); ok {
    return 0
  }
}
