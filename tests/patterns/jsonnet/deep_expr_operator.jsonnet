local f = function () [
  //ERROR:
  foo(bar(1 + 42)),

  foo(1)
];