// from https://kotlinlang.org/docs/generics.html

class Box<T>(t: T) {
    var value = t
}

interface Source<out T> {
    fun nextT(): T
}

