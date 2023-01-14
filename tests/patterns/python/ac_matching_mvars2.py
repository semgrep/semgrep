
#OK:
if A & B:
  foo()

#ERROR:
if A & A:
  foo()

#ERROR:
if A & B & A:
  foo()
