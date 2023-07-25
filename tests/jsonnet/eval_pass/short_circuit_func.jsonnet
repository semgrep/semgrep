// lazy arguments! so no need for macros to express
// cool constructs:
[local and(a, b) = a && b;
and(false, error "this one is never evaluated")
]
