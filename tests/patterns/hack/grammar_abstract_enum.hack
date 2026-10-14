<?hh
// ERROR: match
abstract enum class AbstractEnum : mixed { abstract int Value; int Answer = 42; }
abstract enum class AbstractEnum : mixed { abstract string Value; int Answer = 42; }
enum class AbstractEnum : mixed { int Value = 1; int Answer = 42; }
