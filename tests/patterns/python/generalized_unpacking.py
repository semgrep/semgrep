# PEP 448 generalized ** unpacking in a dict literal (Python 3.5).
d1 = {"x": 1}
d2 = {"y": 2}
# ERROR: match
merged = {**d1, **d2}


single = {**d1}
