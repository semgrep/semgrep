import A.B.C
using .A.B
import A: x as y, (+) as add, @m as @n
import A: x as y, @m as @B.n
import @m as @B.n
using A: (x)
export (x), @m, +
export @B.m
A.B.@m(x, y=1)
@A.B.m x y
A.@m x y
