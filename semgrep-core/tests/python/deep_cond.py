def foo():
    #ERROR: match
    if x == true:
      return 1

    #ERROR: match
    if (b == c) and (x == true):
      return 1
