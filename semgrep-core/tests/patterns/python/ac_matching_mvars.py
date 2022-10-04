
#ERROR:
if A & B:
  foo()

#ERROR:
if B & A:
  foo()

#ERROR:
if A & C:
  foo()

#ERROR:
if A & B & C:
  foo()

#OK:
if B & C:
  foo()
