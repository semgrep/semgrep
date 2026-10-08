def f(payload):
    match payload:
        case {**rest}:
            print(rest)
