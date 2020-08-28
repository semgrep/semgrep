<?hh

// Kitchen sink

function vidx<X>(vector<X> $list, int $idx):X {
  return $list->d[$idx];
}

function pair<X,Y>(X $x, Y $y):(X,Y) { return array($x, $y); }

function car<X,Y>((X,?Y) $pair):X {
  return $pair[0];
}

interface Face<A> {
}

class vector<X> { function __construct($x) { $this->d = $x; } }

function vector<X>(/*...*/):vector<X> {
  return new vector(func_get_args());
}

class Foo<X> implements Face<X> {
  const string BLEH = "b";
}

$blork = pair('c', '-');

//TODO
//function right_shift_hack(Foo<Foo<Foo<Foo<Foo<Foo<Foo<Foo<Foo<Foo<Foo>,Foo>>,Foo>>>,Foo>>>> $bonk, (function(Foo,Bar):C) $d) {
//}

$a = vector('a','aa','aaa');
$d = (function():UNICORNS{return 'd';});
echo vidx($a, 0), Foo::BLEH, car($blork), $d();

abcd;


class Foo {
  public Vector< ?Vector<string> > $y;
  public Vector<?Vector<string> > $y;
  public Vector<Vector<string>> $y;

  // not handled for now :(
  // public Vector< ?Vector<string>> $y;
}
