
#OK:
if A or B:
  foo()

# OK
if A or C:
  foo()

#ERROR:
if A or (B or B):
  foo()

#ERROR:
if (A or B) or B:
  foo()

#OK:
if (A or B) or B or C:
  foo()

#OK:
if B or (B or C):
  foo()
