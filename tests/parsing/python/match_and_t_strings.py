match event:
    case Click(value):
        message = t"clicked {value}"
    case _:
        message = t"unknown"
