def foo():
    #ERROR: match
    try:
        foo()
    # PatVar
    except ValidationError as e:
        return e

