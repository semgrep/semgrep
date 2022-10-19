# Forked from metavar_string.py to comment out the f string below but test
# autofix on the other cases. Once f string autofix works, we should test
# autofix on metavar_string.py and remove this test case.

def test():
    #ERROR: match
    foo("a string")
    #ERROR: match
    foo('another string')
    #ERROR: match
    foo("\"escaped string\"")

    # TODO fix autofix on this case and then reconcile with the
    # metavar_string.py test case
    #
    # foo(f"an fstring")

    #ERROR: match
    foo("""a multiline string""")
    #ERROR: match
    foo('singlequote with " inside')
    #ERROR: match
    foo("doublequote with ' inside")

    # this nope
    foo(1)

