# ruleid: dict-key-ellipsis
a = {"k": 1}

# ruleid: dict-key-ellipsis
b = {"k": 1, "other": 2}

# ruleid: dict-key-ellipsis
c = {"k": 1, **d}

# ruleid: dict-key-ellipsis
e = {**d, "k": 1}

# ok: dict-key-ellipsis
f = {"other": 2, **d}

# ok: dict-key-ellipsis
g = {**d}
