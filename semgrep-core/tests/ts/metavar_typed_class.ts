class C {
  f() {}
}

class D extends C { }

const c1: C = new C();
const c2 = new C();

const d1: D = new D();
const d2 = new D();

// MATCH:
c1.f();
// MATCH:
c2.f();
// MATCH:
(new C()).f();

// DEEP:
d1.f();
// DEEP:
d2.f();
// DEEP:
(new D()).f();
