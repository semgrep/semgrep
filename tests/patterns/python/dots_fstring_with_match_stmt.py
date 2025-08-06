def foo():
  #ERROR: match
  print(f"this is good")

  # ERROR: match
  print(f"this should {match}")

  print("hello")

  print("hello" "world")

match status:
    case 400:
        return
