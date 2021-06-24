
#OK:
if A or B:
  foo()

#ERROR:
if (A or B) or C:
  foo()

#ERROR:
if A or (B or (C or (D or E))):
  foo()
