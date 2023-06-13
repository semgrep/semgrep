#OK:
@decorator
def fun():
  return 42

#OK:
@decorator(1,2,3)
def grumble():
  return "wahoo"

#OK:
@decorator("oh")
@decorator("my")
def grr():
  return 0

