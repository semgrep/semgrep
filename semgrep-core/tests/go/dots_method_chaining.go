func foo() {
    
  //ERROR: match
  f = o.foo().m().h().bar().z()

  //ERROR: match
  f = o.foo().bar()

  // this one does not contain the bar()
  f = o.foo().m().h().z()

  //ERROR: match $O can match o.before()
  f = o.before().foo().m().h().bar().z()

}
