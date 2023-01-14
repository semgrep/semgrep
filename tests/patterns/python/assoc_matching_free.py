
#OK:
if A and B:
  foo()

#ERROR:
if (A and B) and C:
  foo()

#ERROR:
if A and (B and C):
  foo()

#ERROR:
if A and B and C:
  foo()

#OK:
if B and A and C:
  foo()

#OK:
if C and A and B:
  foo()
