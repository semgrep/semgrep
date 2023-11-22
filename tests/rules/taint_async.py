import aiohttp

headers4={"Authorization": "Basic bG9naW46cGFzcw=="}

#ruleid: test
with aiohttp.ClientSession(headers=headers4) as session:
    async with session.get("http://httpbin.org/headers") as r:
        json_body = await r.json()
        assert json_body['headers']['Authorization'] == \
            'Basic bG9naW46cGFzcw=='

def test():
    #ruleid: test
    async with aiohttp.ClientSession(headers=headers4) as session:
        async with session.get("http://httpbin.org/headers") as r:
            json_body = await r.json()
            assert json_body['headers']['Authorization'] == \
                'Basic bG9naW46cGFzcw=='
