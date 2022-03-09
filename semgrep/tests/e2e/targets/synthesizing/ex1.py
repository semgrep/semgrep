from metrics import send


def foo():
    bar = 3
    bar + send("my-report-id")
