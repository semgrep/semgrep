<?hh

// generic classes
class A<T1, T2> {
  public function foo(T1 $x): T2 {
  }
}

class C<T> { }
interface I<T> { }
trait T<T> { }

// generic functions
function id<T>(T $x): T {
  return $x;
}

// use of type arguments

class B extends A<int, float> {
  const A<int, float> CST = 0;
  public A<int,float> $fld;
  public function bar(A<int, float> $x) {
  }
}

function bar(A<int, float> $x) {
}


// note that type arguments are allowed only in definition context,
// not in expression context. So the following is invalid:
//  $o = new A<int,float>();
//  try { ... } catch (Exception<string> $x) { ... }
//  if($x instanceof A<int,float>) { ... }
