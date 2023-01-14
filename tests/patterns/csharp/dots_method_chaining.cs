class Foo {

  void test() {

    //ERROR: match
    f = this.foo().m().h().bar().z();

    //ERROR: match
    f = this.foo().bar();

    f = this.foo().m().h().z();

    //ERROR: match $O can match o.before()
    f = this.before().foo().m().h().bar().z();
  }
}

