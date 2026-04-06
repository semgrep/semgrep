
enum Side derives Eq:
  case KingSide, QueenSide

enum Color derives Eq, Ordering:
  case Red, Green, Blue

enum Planet(mass: Double, radius: Double) extends java.lang.Enum[Planet] derives Eq:
  case Mercury extends Planet(3.303e+23, 2.4397e6)

// Test: derives on a class (no extends)
class Point(x: Int, y: Int) derives Eq, Ordering
