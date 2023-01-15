# https://github.com/returntocorp/semgrep/issues/3489

#ruleid:test
"bad string"

#OK:test
@function_that_makes_it_okay("bad string") # shouldn't be flagged
def foo():
    #ruleid:test
    "bad string" # should be flagged

def bar():
    #ruleid:test
    "bad string"

#OK:test
bar = function_that_makes_it_okay("bad string")(bar)
