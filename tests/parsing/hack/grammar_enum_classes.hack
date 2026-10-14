<?hh
class ExampleClass {}
abstract enum class AbstractEnum : mixed {
  int X = 42;
  string S = 'foo';
  abstract ExampleClass C;
}
enum class ConcreteEnum : mixed extends AbstractEnum {
  ExampleClass C = new ExampleClass();
}
$qualified = ConcreteEnum#C;
$unqualified = #C;
