def generate_to_ten():
    x = 0
    while x <= 10:
        yield x
        x += 1

def defer_to_generate_to_ten():
    yield from generate_to_ten()

def use_yield_expr():
    await (yield 3)

def use_yield_from_expr():
    await (yield from defer_to_generate_to_ten())
