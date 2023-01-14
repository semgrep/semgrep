def update_system(password):
    if cond():
        password = "abc"
    # OK: it's not guaranteed to be "abc"!
    set_password(password)

