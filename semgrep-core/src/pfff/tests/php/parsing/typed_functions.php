<?hh

function id(?A $x): int {
  return $x;
}

function foo(mixed $x): int { }

function id((int, float) $x): int { return 1; }
