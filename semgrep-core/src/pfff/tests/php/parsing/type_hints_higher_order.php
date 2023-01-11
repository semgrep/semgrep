<?hh

function f1():
 (function(string): string) {
  return null;
}

function f2(string $name, (function(...):mixed) $func) {
}
