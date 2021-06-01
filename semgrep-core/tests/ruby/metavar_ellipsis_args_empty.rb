#ERROR:
foo(abcd, frobnicate: true)
#ERROR:
foo(abcd, "efgh", frobnicate: true)
#ERROR: this one also! $...ARGS should also match the empty list
foo(ijkl)
#ERROR:
foo("mnop", frobnicate: false)
#ERROR:
foo("qrst", "uvwx")
