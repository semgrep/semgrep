
#ERROR:
if A and B:
  foo()

#ERROR:
if C and B and A:
  foo()

#OK:
if A and A:
  foo()
