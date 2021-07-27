
#ERROR:
if A and B:
  foo()

#ERROR:
if A and C and B:
  foo()

#ERROR:
if A and C and D and B:
  foo()

#OK:
if A and A:
  foo()
