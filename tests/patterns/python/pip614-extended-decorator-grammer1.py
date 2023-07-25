#ERROR:
@why := did[python].allow.this[111]
def function():
  return None

#ERROR:
@why := lambda x: x
def grr():
  return None

#OK:
@iwonder
def grumble():
  return "grrrrr"

