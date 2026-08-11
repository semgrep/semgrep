def f():
    # The inner metavariable-pattern binds $Y, which is interpolated into the
    # message. Regression target for ENGINE-2932: exactly one finding, not a
    # bare duplicate whose message still reads "found secret argument $Y".
    log(secret("password"))
