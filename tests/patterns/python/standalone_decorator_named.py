#ERROR:
@art("deco")
class room():
  #OK:
  def desk():
    return None

#OK
@empty_deco
#ERROR:
@arg_deco("semgrep")
def decorate():
  return "flowers!"
