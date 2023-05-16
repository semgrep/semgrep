# MATCH:
try:
    result = 10 / 0 # division by zero
except Exception as e:
    print("handled") # code to handle the exception