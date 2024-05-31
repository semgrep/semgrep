# ruleid: decorator-order-matters
@first("syn")
@second("ack")
def func1():
    pass


@second("ack")
@first("syn")
def func2():
    pass
