MAKE_BAR_INSECURE = True

def foo():
    # ruleid: anywhere-global
    bar()
    foo()
    if (MAKE_BAR_INSECURE):
        print("Hello, world!")
    return 2

# ruleid: anywhere-global
bar()
