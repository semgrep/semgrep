

def alwaysblue():
    if isblue():
        return 'blue'
    # ruleid: code-after-unconditional-return
    return 'red'
    return 'green'


def alwaysblue():
    if isblue():
        return 'blue'
    # ruleid: code-after-unconditional-return
    return 'red'
    x = 5


def resolve(key: str):
    key = os.path.join(path, "keys", key)
    # ok
    return key


def resolve(key: str) -> str:
    key = os.path.join(path, "keys", key)
    # ok
    return key

def resolve(key: str) -> str:
    key = os.path.join(path, "keys", key)
    # ok
    return key, key

# ruleid: return-not-in-function
return (a, b)
