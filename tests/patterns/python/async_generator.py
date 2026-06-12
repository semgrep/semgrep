# PEP 525 async generators (Python 3.6).
# ERROR: match
async def agen():
    yield 1
    yield 2


# ERROR: match
async def agen_with_arg(x):
    yield x


# OK:
def gen():
    yield 1
