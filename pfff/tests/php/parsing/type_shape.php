<?hh

type ashape = shape(
  'foo' => int,
  'bar' => string
);

$o = shape(
  'foo' => 1,
  'bar' => "bla",
);


class X {
  const string X1 = 'field1';
  const string X2 = 'field2';
}

type myshape = shape(X::X1 => int, X::X2 => bool);
$x = shape(X::X1 => 1, X::X2 => true);
