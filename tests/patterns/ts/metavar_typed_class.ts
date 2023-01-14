class C {
  f() {}
}

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
