# This test aims to exercise various exception paths.

def throw_exits(input):
  clean = None
  raise RuntimeError()

  # ok:
  sink = clean

def throw_in_catch_exits(input):
  clean = None
  try:
    raise RuntimeError()
  except Exception as e:
    raise RuntimeError()

  #ok: unreachable, because throw inside the catch clause exits
  sink = clean

def throw_in_else_exits(input):
  clean = None
  try:
    pass
  except Exception as e:
    clean = input
    #ok: unreachable, because exception was not thrown
    sink = clean
  else:
    raise RuntimeError()

  #ok: unreachable, because throw inside the else clause exits
  sink = clean

def throw_in_finally_exits(input):
  clean = None
  try:
    pass
  finally:
    raise RuntimeError()

  #ok: unreachable, because throw inside the finally clause exits
  sink = clean

def return_exits(input):
  clean = None
  return

  # ok:
  sink = clean

def return_in_catch_exits(input):
  clean = None
  try:
    raise RuntimeError()
  except Exception as e:
    return

  #ok: unreachable, because return inside the catch clause exits
  sink = clean

def return_in_else_exits(input):
  clean = None
  try:
    pass
  except Exception as e:
    clean = input
    #ok: unreachable, because exception was not thrown
    sink = clean
  else:
    return

  #ok: unreachable, because return inside the else clause exits
  sink = clean

def throw_in_finally_exits(input):
  clean = None
  try:
    pass
  finally:
    return

  #ok: unreachable, because return inside the finally clause exits
  sink = clean

def throw_must_not_go_through_else(input):
  clean = None
  dirty = None
  try:
    raise RuntimeError()
  except Exception as e:
    # assume this path may be taken
    dirty = input
  else:
    # no flow through here because an exception was thrown
    clean = input

  #ok:
  sink = clean
  #ruleid: python-exception
  sink = dirty

def no_throw_goes_through_else(input):
  clean = None
  dirty = None
  try:
    pass
  except Exception as e:
    # no flow through here because no exception was thrown
    clean = input
  else:
    dirty = input

  #ok:
  sink = clean
  #ruleid: python-exception
  sink = dirty

def may_throw_goes_through_catch_and_else(input):
  dirty1 = None
  dirty2 = None
  try:
    any_function_call_may_raise()
  except Exception as e:
    # exception may be thrown
    dirty1 = input
  else:
    # exception may not be thrown either
    dirty2 = input

  #ruleid: python-exception
  sink = dirty1
  #ruleid: python-exception
  sink = dirty2

def exception_or_not_goes_through_finally(input):
  clean1 = None
  clean2 = None
  try:
    any_function_call_may_raise()
  except Exception as e:
    clean1 = input
  else:
    clean2 = input
  finally:
    clean1 = sanitize
    clean2 = sanitize

  #ok:
  sink = clean1
  #ok:
  sink = clean2

def exception_or_not_goes_through_finally(input):
  clean1 = None
  clean2 = None
  try:
    any_function_call_may_raise()
  except Exception as e:
    clean1 = input
  else:
    clean2 = input
  finally:
    clean1 = sanitize
    clean2 = sanitize

  #ok:
  sink = clean1
  #ok:
  sink = dirty2

def return_goes_through_finally_and_propagates(input):
  clean1 = None
  clean2 = None
  clean3 = None
  clean4 = None
  dirty1 = None
  dirty2 = None
  try:
    try:
      return
    except Exception as e:
      clean1 = input
      #ok: unreachable because the try clause returns
      sink = clean1
    else:
      clean2 = input
      #ok: unreachable because the try clause returns
      sink = clean2
    finally:
      # Finally clauses are always reachable when present
      dirty1 = input

    clean3 = input
    #ok: unreachable because the inner try is returning
    sink = clean3
  except Exception as e:
    clean4 = input
    #ok: unreachable because the inner try didn't throw an exception
    sink = clean4
  finally:
    dirty2 = input

  #ruleid: python-exception
  sink = dirty1
  #ruleid: python-exception
  sink = dirty2

def throw_may_go_through_catch_and_propagates(input):
  clean1 = None
  clean2 = None
  clean3 = None
  dirty1 = None
  dirty2 = None
  dirty3 = None
  try:
    try:
      raise RuntimeError()
    except Exception as e:
      clean1 = input
      #ruleid: python-exception
      sink = clean1
    else:
      clean2 = input
      #ok: unreachable because the try clause returns
      sink = clean2
    finally:
      # Finally clauses are always reachable when present.
      dirty1 = input
    # This is reachable when the exception isn't handled.
    clean3 = input
  except Exception as e:
    # The inner exception may be propagated here.
    dirty2 = input
  finally:
    dirty3 = input

    #ruleid: python-exception
    sink = dirty1
    #ruleid: python-exception
    sink = dirty2
    #ruleid: python-exception
    sink = dirty3
