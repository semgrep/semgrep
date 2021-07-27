
#ERROR:
if A or B:
  foo()

#ERROR:
if (C or A) or B:
  foo()

#ERROR:
if (A or B) or C or D:
  foo()

#OK:
if A or A:
  foo()
