def f()
  nil
  # ERROR:
  "implicit return last statement"
end

def f(x)
  if x < 0
    # ERROR:
    "implicit return if"
  elsif x == 0
    # ERROR:
    "implicit return elsif"
  else
    # ERROR:
    "implicit return else"
  end
end

def f(x)
  if x == 0
    "no implicit return because not the last statement"
  else
    "no implicit return because not the last statement"
  end
  # ERROR:
  "implicit return last statement after non trivial"
end

def f()
  def f()
    nil
    # ERROR:
    "implicit return nested function even when function is not the last statement"
  end
  # ERROR:
  "implicit return last statement after nested function"
end

def f()
  begin
    function_call_may_throw_exception()
    # ERROR:
    "implicit return in try clause"
  rescue Error=>e
    # ERROR:
    "implicit return in catch clause"
  end
end

def f()
  begin
    "no implicit return in try clause if ensure is present"
  rescue Error=>e
    "no implicit return in catch clause if ensure is present"
  ensure
    # ERROR:
    "implicit return in ensure clause"
  end
end

class C
  def f()
    # ERROR:
    "implicit return function inside class"
  end
end

module M
  def f()
    # ERROR:
    "implicit return function inside module"
  end
end

def f()
  # ERROR:
  return "explicit return should still work"
end

def f(x)
  case x
  when 0
    # ERROR:
    "implicit return case"
  else
    # ERROR:
    "implicit return else case"
  end
end
