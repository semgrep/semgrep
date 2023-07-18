// https://github.com/returntocorp/semgrep/issues/3484

// these should match:

//ERROR:
import "foobar"
//ERROR:
import (
    "foobar"
)
//ERROR:
import ("foobar/foofunc")
//ERROR:
import (
    foobar "foobar"
)

// these should not match:

//OK:
import "foobar4"
//OK:
import (
    "foobar4"
)
//OK:
import ("foobar4/foofunc")
//OK:
import (
    foobar "foobar4"
)
//OK:
import "_foobar"
//OK:
import (
    "_foobar"
)
//OK:
import ("_foobar/foofunc")
//OK:
import (
    foobar "_foobar"
)
