# PEP 492 coroutines: async def / await / async with / async for (Python 3.5).

async def co():
    return 1


async def caller():
    # await on a call.
    x = await co()
    # await on a parenthesized expression.
    y = await (co())
    return x + y


class AsyncCM:
    async def __aenter__(self):
        return self
    async def __aexit__(self, exc_type, exc, tb):
        return False


class AsyncIter:
    def __aiter__(self):
        return self
    async def __anext__(self):
        raise StopAsyncIteration


async def use_async_with():
    async with AsyncCM() as cm:
        return cm


async def use_async_with_multi():
    async with AsyncCM() as a, AsyncCM() as b:
        return a, b


async def use_async_for():
    async for item in AsyncIter():
        pass


async def nested():
    async with AsyncCM() as cm:
        async for item in AsyncIter():
            x = await co()
            return cm, item, x
