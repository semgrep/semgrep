def foo():
  #ERROR: match
  print(f"this is good")

  # ERROR: match
  print(f"this should {match}")

  # ERROR: match
  print(f"")

  print("hello")

  print("hello" "world")

  print("")

match status:
    case 400:
        return
