<?hh
// ERROR: match
enum class ConcreteEnum : mixed extends AbstractEnum { int Value = 42; }
enum class ConcreteEnum : mixed extends AbstractEnum { string Value = "42"; }
enum class ConcreteEnum : mixed extends OtherEnum { int Value = 42; }
