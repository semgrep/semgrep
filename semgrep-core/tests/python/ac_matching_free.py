
#ERROR:
if A & B:
  foo()

#ERROR:
if B & A:
  foo()

# OK
if A & C:
  foo()

#ERROR:
if A & B & B:
  foo()

#ERROR:
if A & B & C:
  foo()

#ERROR:
if A & (B & C):
  foo()

#ERROR:
if C & A & B:
  foo()

#ERROR:
if B & A & C:
  foo()

#ERROR:
if A & C & B:
  foo()
