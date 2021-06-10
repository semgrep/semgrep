
#ERROR:
if A and B:
  foo()

#ERROR:
if B and A:
  foo()

# OK
if A and C:
  foo()

#ERROR:
if A and B and B:
  foo()

#ERROR:
if A and B and C:
  foo()

#ERROR:
if A and (B and C):
  foo()

#ERROR:
if C and A and B:
  foo()

#ERROR:
if B and A and C:
  foo()

#ERROR:
if A and C and B:
  foo()
