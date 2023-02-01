local f = function () [
 //ERROR:
    foo(1,2),

 //ERROR:
 foo(1,
     2),

 //ERROR:
 foo (1, // comment
      2),

 // nope, not this one
 foo(2,1),
];
f()
