# PEP 525 async generators and PEP 530 async comprehensions (Python 3.6).


async def agen():
    yield 1
    yield 2


async def agen_with_value(x):
    if x:
        yield x
    yield 0


async def consume():
    # async list comprehension.
    xs = [x async for x in agen()]
    # async set comprehension.
    ys = {x async for x in agen()}
    # async dict comprehension.
    zs = {x: x async for x in agen()}
    # async generator expression.
    g = (x async for x in agen())
    # Mixed async-for + regular if filter.
    filtered = [x async for x in agen() if x > 0]
    # Mixed: outer async-for with inner sync for.
    nested = [(x, y) async for x in agen() for y in [1, 2]]
    return xs, ys, zs, g, filtered, nested


async def await_inside_comprehension():
    # Await inside an async comprehension.
    return [await agen_with_value(i).__anext__() async for i in agen()]
