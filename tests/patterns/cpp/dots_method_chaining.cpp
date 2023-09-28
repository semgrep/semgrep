void test() {

  //ERROR: match
  int f = this.foo().m().h().bar().z();

  //ERROR: match
  int f = this.foo().bar();

  int f = this.foo().m().h().z();

  //ERROR: match $O can match o.before()
  int f = this.before().foo().m().h().bar().z();
}