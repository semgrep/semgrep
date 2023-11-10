void foo() {
  void* curBase;
  size_t curLen;

  while (curIov < count) {                    // (3) <flow> make sure we mark when we are in the loop
    ssize_t res1 = 0;
    ssize_t res2 = 0;

    if (isRead) {
      res1 = SOURCE();                        // (1) taint source ~~> res1
    } else {
      res2 = pass_the_taint(curBase);         // (4) curBase is tainted, then we mark res2 as tainted again
    }

    curBase = (void*)((char*)curBase + res1); // (2) res1 is tainted, then we taint curBase

    SINK(res2);                               // (5) sink res2
  }
}
