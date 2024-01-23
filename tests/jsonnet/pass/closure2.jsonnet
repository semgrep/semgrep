local z = 1;

local adder(x) = (function (y) x + y + z);

local f = [ adder(2) ];

f[0](3)
