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
#ERROR:
@decorator("my")
def grr():
  return 0

#ERROR:
@decorator(10)
class ifed():
  #OK:
  def __init__():
    return None

#OK:
class room(1,2,3):
  #ERROR:
  @look("up")
  def __init__():
    return None

