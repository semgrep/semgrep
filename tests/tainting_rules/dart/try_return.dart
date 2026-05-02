// Translated from java/try_return.java

class Main {
  // These simulate the Java helper methods
  String? couldThrow() {
    // may throw
    return null;
  }

  String source() {
    return "tainted";
  }

  void sink(String s) {
    print("sink: $s");
  }

  // -------------------------
  // test1
  // -------------------------
  String? test1() {
    String str = "safe";
    try {
      // Java: return could_throw();
      // Dart: method returns String?, so this is valid
      return couldThrow();
    } catch (e) {
      str = source();
    }

    // ruleid: test
    sink(str);
    return null;
  }

  // -------------------------
  // test2
  // -------------------------
  int? test2() {
    int cannotThrow = 42;
    String str = "safe";

    try {
      // Java: return cannot_throw;
      // Dart: method returns int?, so this is valid
      return cannotThrow;
    }
    // vvv dead code (preserved exactly)
    catch (e) {
      str = source();
    }

    // OK:
    sink(str);
    return null;
  }
}
