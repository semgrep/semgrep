
#ERROR:
if A and B:
  foo()

#OK:
if B and A:
  foo()

#ERROR:
if A and C:
  foo()

#ERROR:
if (A and B) and C:
  foo()

#ERROR:
if (A and B) and (C and D):
  foo()

#OK:
if B and C:
  foo()
