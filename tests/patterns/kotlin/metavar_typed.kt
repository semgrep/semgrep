class Foo {
    fun main() {
      val x : String
      val y : String
      val a : Int
      val b : Int
      //ERROR: match
      if (x == y) x = y
      if (a == b) a = b
   }
}
