#ERROR: match
class A:
  def foo():
    return 1

#ERROR: match
class A():
  def foo():
    return 1

#ERROR: match
class A(B):
  def foo():
    return 1
