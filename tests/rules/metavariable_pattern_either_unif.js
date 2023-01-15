function test() {

   let a = b;
   // ruleid: metavariable-pattern-either-unif 
   count("hello") + 1;

   let a = b;
   // ok:
   count(b) + 1;

   const a = b;
   // ok:
   count(b) + 1;

   a = b;
   // ok:
   count(b) + 1;
}
