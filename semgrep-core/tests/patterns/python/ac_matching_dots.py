
#ERROR:
if A & B:
  foo()

#ERROR:
if C & B & A:
  foo()

#OK:
if A & A:
  foo()
