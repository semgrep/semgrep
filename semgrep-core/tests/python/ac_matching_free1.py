
#OK:
if A and B:
  foo()

# OK
if A and C:
  foo()

#ERROR:
if A and B and B:
  foo()

#ERROR:
if A and B and C and B:
  foo()

#OK:
if B and (B and C):
  foo()
