# ERROR:
f() = "implicit return for function definition without function keyword"

# ERROR:
() -> "implicit return for lambda"

function f()
  nothing
  # ERROR:
  "implicit return last statement"
end

function f(x)
  if x < 0
    # ERROR:
    "implicit return if"
  elseif x == 0
    # ERROR:
    "implicit return elsif"
  else
    # ERROR:
    "implicit return else"
  end
end

function f(x)
  if x == 0
    "no implicit return because not the last statement"
  else
    "no implicit return because not the last statement"
  end
  # ERROR:
  "implicit return last statement after non trivial"
end

function f()
  function f()
    nil
    # ERROR:
    "implicit return nested function even when function is not the last statement"
  end
  # ERROR:
  "implicit return last statement after nested function"
end

function f()
  try
    # ERROR:
    "implicit return in try clause"
  catch e
    # ERROR:
    "implicit return in catch clause"
  end
end

function f()
  try
    "no implicit return in try clause if ensure is present"
  catch e
    "no implicit return in catch clause if ensure is present"
  finally
    # ERROR:
    "implicit return in ensure clause"
  end
end

module M
  function f()
    # ERROR:
    "implicit return function inside module"
  end
end

function f()
  # ERROR:
  return "explicit return should still work"
end
