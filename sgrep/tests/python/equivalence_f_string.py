# ERROR:
w = "foo"
query = f"hello {w} hel"

# ERROR:
w = "bar"
query = f"ASD{ww}ASASD"

# ERROR:
www = "bar"
query = f".SS{www}"

# ERROR:
www = "bar"
query = f".SS{www} and {ww} aasd {w}"

# OK:
num = 1
query = f"num = {num} !"

# OK:
f = foo()
query = f"complex = {f} !"