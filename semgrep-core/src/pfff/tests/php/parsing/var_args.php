<?hh

function f1(...): void { }

function f2(int $x, ...): void { }

class Foo {
  public function f3(...): void { }

  public function f4(string $x, int $y, bool $z, ...): void { }
}
