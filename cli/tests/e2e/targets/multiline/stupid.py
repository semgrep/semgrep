SUPER_LONG_CONSTANT_TO_TRIGGER_A_LINE_BREAK = 0


def foo(a, b):
    # ERROR: stupid $X == $X
    return (
        SUPER_LONG_CONSTANT_TO_TRIGGER_A_LINE_BREAK
        == SUPER_LONG_CONSTANT_TO_TRIGGER_A_LINE_BREAK
    )

x = """
    abc
    def
"""