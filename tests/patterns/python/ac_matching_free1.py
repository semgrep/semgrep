
#OK:
if A | B:
  foo()

# OK
if A | C:
  foo()

#ERROR:
if A | B | B:
  foo()

#ERROR:
if A | B | C | B:
  foo()

#OK:
if B | (B | C):
  foo()
