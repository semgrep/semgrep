# ok: dict-key-splat
a = {"k": 1}

# ok: dict-key-splat
b = {"k": 1, "other": 2}

# ruleid: dict-key-splat
c = {"k": 1, **d}

# ruleid: dict-key-splat
e = {**d, "k": 1}

# ok: dict-key-splat
f = {"other": 2, **d}

# ok: dict-key-splat
g = {**d}
