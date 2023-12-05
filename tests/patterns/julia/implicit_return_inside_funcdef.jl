# ERROR:
function f()
  nothing
  "implicit return last statement"
end

# ERROR:
function f(x)
  if x < 0
    "implicit return if"
  elseif x == 0
    "implicit return elsif"
  else
    "implicit return else"
  end
end

# ERROR:
function f(x)
  if x == 0
    "no implicit return because not the last statement"
  else
    "no implicit return because not the last statement"
  end
  "implicit return last statement after non trivial"
end

# ERROR:
function f()
  # ERROR:
  function f()
    nil
    "implicit return nested function even when function is not the last statement"
  end
  "implicit return last statement after nested function"
end

# ERROR:
function f()
  try
    "implicit return in try clause"
  catch e
    "no implicit return in catch clause if there is no exception"
  end
end

# ERROR:
function f()
  try
    "no implicit return in try clause if else clause is present"
  catch e
    "no implicit return in catch clause if there is no exception"
  else
    "implicit return in else clause if there is no exception in try clause"
  end
end

# ERROR:
function f()
  try
    "no implicit return in try clause if ensure is present"
  catch e
    "no implicit return in catch clause if ensure is present"
  finally
    "implicit return in ensure clause"
  end
end

# ERROR:
function f()
  try
    "no implicit return in try clause if ensure is present"
  catch e
    "no implicit return in catch clause if ensure is present"
  else
    "no implicit return in else clause if ensure is present"
  finally
    "implicit return in ensure clause"
  end
end

module M
  # ERROR:
  function f()
    "implicit return function inside module"
  end
end

# ERROR:
function f()
  return "explicit return should still work"
end
