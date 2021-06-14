def test():
    #ERROR: match
    foo("a string")
    #ERROR: match
    foo('another string')
    #ERROR: match
    foo("\"escaped string\"")
    #ERROR: match
    foo(f"an fstring")
    #ERROR: match
    foo("""a multiline string""")

    # this nope
    foo(1)

