class C {
  int M(bool c) {
    // `#if` alternatives are not a conditional expression: no match.
    var x = 1 +
#if A
      2
#else
      3
#endif
      ;
    // ERROR:
    return c ? 1 : 2;
  }
}
