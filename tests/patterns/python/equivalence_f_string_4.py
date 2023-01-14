def foo(x):
    A = x
    # ERROR:
    B = x + x
    f"{A} {B}"
