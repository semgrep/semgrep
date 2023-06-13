#ERROR:
@decorator
def fun():
  return 42

#ERROR:
@decorator(1,2,3)
def grumble():
  return "wahoo"

#ERROR:
@decorator("oh")
@decorator("my")
def grr():
  return 0

