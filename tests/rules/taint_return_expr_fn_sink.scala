object Demo {
   def test(arg1: Int, arg2: Int) : Stirng = {
      var a: Int = arg1
      var b: Int = arg2
      // ruleid: simple-function-sink
      return "Returned Value : " + addInt(a, b)
   }
   
   def addInt( a:Int, b:Int ) : Int = {
      var sum:Int = 0
      sum = a + b

      return sum
   }
}
