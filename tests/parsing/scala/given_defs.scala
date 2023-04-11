
// GivenType case

given foo: int 

given bar[foo : T]: string

given bar[foo <: T1 : T, qux <: T2 :> T2 : T3]: string

given bar (using foo : int = 3, bar : string): string

given bar (using foo : int = 3) (using string, bool => bool): string

given bar (using foo : int = 3) (using string, bool => bool): string = 2

given bar: int @qux 

// GivenStructural case

given bar: int @qux (1, 2, 3) 

given bar: int @qux (1, 2, 3) (using 3, 4, 5) 

given bar: int with string with bool

given bar: int with string with {
  val x = 2 
}

given bar: int with string with
  val x = 2

object foo:
  given bar: int with string with
    val x = 2