from flask import make_response, request

def test1():
    # ruleid: response-contains-unsanitized-input
    x = request.args.get("x")
    return make_response("found {}".format(x))


def test1():
    # ruleid: response-contains-unsanitized-input
    x = request.args.get("x")
    y = make_response("found {}".format(x))
    return y


def test2():
    # ok
    x = request.args.get("x")
    y = some_safe_operation_on(x)
    return make_response("found {}".format(y))


def test3():
    # ruleid: response-contains-unsanitized-input
    x = request.args.get("x")
    return make_response(f"found {x}")
