    
//ERROR: match
def f = o.foo().m().h().bar().z()

//ERROR: match
def f = o.foo().bar()

  // this one does not contain the bar()
def f = o.foo().m().h().z()

  //ERROR: match $O can match o.before()
def f = o.before().foo().m().h().bar().z()

