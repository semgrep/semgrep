# PEP 492 async def / await (Python 3.5).
async def caller():
    # ERROR: match
    x = await co()
    return x
