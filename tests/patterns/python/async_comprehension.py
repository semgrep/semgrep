# PEP 530 async comprehensions (Python 3.6).
async def consume():
    # ERROR: match
    xs = [x async for x in agen()]
    return xs
