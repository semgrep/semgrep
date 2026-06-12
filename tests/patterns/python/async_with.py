# PEP 492 async with (Python 3.5).
async def use():
    # ERROR: match
    async with cm() as c:
        return c


async def plain():
    # OK:
    with cm() as c:
        return c
