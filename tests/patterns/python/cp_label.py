# This is not actually valid Python. Python does not allow comments after line
# continuations. But, we don't have any other reasonable way to verify that the
# string itself matches, but not the variable to which it is assigned.
#
# Luckily, our current parser accepts this even though the actual Python parser
# does not. But, if that ever changes in the future it's probably fine to delete
# this test and rely on the cp_label.js test which tests the same behavior.

#OK:
a =\
    #ERROR:
    "foo"

#OK:
a = "bar"

def f():
    #OK:
    c =\
        #ERROR:
        "foo"
