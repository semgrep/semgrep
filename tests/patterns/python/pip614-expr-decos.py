@$NAME.$FIELD[$EXPR](...)

#OK:
@reg.ular.deco.rat.or
def f0():
  return None

#OK:
@boring
def f1:
  return None

#OK:
@boring[122]
def f2:
  return None

#ERROR:
@why.do["computers"]("hate me")
def f3:
  return None

#ERROR:
@sem.grep["lambda"](1,3,4,5,6)
def f4:
  return None

#ERROR:
@a.b["wefwef"]()
def f5:
  return None
