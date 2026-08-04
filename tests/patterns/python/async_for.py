# PEP 492 async for (Python 3.5).
async def use():
    # ERROR: match
    async for item in aiter():
        print(item)


async def plain():
    # OK:
    for item in it():
        print(item)
