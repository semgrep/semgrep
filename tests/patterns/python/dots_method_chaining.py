def foo():

    # ERROR: match
    o.foo().m().h().bar().z()

    # ERROR: match
    o.foo().bar()

    # this one does not contain the bar()
    o.foo().m().h().z()

    # ERROR: match $O can match o.before()
    o.before().foo().m().h().bar().z()
