
#OK:
if A and B:
  foo()

#ERROR:
if A and A:
  foo()

#ERROR:
if A and B and A:
  foo()
