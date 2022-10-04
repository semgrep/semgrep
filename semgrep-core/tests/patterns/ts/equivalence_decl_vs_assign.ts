class A {
      //ERROR: match
      fld = foo();

      foo() {
         return 1;
      }
}

// this should not match, it does not look like an assign with the colon
o = { fld: foo(), foo() { return 1 }, };
