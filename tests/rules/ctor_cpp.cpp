class Test {

  // ruleid: cpp-match-func-def
  foo bar(x);
  // ruleid: cpp-match-func-def
  foo bar(x, y);

  void test() {
    // ruleid: cpp-match-ctor
    foo bar(1);
    // ruleid: cpp-match-ctor
    foo bar(1, 2);

    // ruleid: cpp-match-ctor
    foo bar(x);
    // ruleid: cpp-match-ctor
    foo bar(x, y);

    // ruleid: cpp-match-ctor
    foo bar(x, 2);
    // ruleid: cpp-match-ctor
    foo bar(1, y);

    // ruleid: cpp-match-ctor-3
    foo bar(3);
    // ruleid: cpp-match-ctor-3
    foo bar(3, 4);
    // ruleid: cpp-match-ctor-3
    foo bar(3, y);
  }
};
