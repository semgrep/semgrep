# PEP 701 f-string formalization — nested same-type quotes (Python 3.12).
d = {"k": 1}
# ERROR: match
a = f"{d["k"]}"


plain = d["k"]
