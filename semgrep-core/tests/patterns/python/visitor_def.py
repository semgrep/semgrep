#ERROR: match
def __eq__(self, other):
  pass

class A:
  #ERROR: match
  def __eq__(self, other):
     pass

def A():
   #ERROR: match
   def __eq__(a, b):
      pass
   return __eq__
