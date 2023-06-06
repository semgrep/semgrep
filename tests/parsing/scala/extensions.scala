

extension (a: T) def x = 2 

object IsProxy:
  extension (a: T) def x = 2 
  extension[T] (a: T) def x = 2 
  extension[T] (using x: A) (a: T) def x = 2 
  extension[T] (using x: A) (a: T) def x = 2 
  extension[T] (using x: A) (a: T) (using y: B) def x = 2 
  extension[T] (using x: A) (a: T) (using y: B) 
    def x = 2
    val y = 3
    export this
  extension[T] (using x: A) (a: T) (using y: B) {
    def x = 2
    val y = 3
    export this
  }

val x = {
  extension (a: T) def x = 2
}