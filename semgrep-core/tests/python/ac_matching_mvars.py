
#ERROR:
if A and B:
  foo()

#ERROR:
if B and A:
  foo()

#ERROR:
if A and C:
  foo()

#ERROR:
if A and B and C:
  foo()

#OK:
if B and C:
  foo()
