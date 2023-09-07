
from A.B import *

# ERROR:
foo(C.D.x)

foo(D.x)

foo(B.C.D.x)

# ERROR:
foo(A.B.C.D.x)

foo(x)