## [1.100.0](https://github.com/semgrep/semgrep/releases/tag/v1.100.0) - 2024-12-12


### Added


- Pro engine now correctly distinguishes overloaded Scala methods based on their
  arity and parameter types, e.g., `foo(x: Int, y: String)` vs. `foo(x: String,
  y: Int)`. (code-7870)


### Changed


- The minimum Python version for semgrep is now 3.9.
  We are dropping support for Python 3.8 (python)


### Fixed


- pro: Fixed a bug in interprocedural index-sensitive taint analysis that caused
  false negatives when a function updated an arbitrary index, e.g.:

      var x = {};

      function foo(k) {
          x[k] = source();
      }

      function test(k) {
          foo(k);
          sink(x); // finding here!
      } (CODE-7838)
- Fixed bug affecting taint tracking through static fields when mixing accesses
  using the class name and using an instance object, e.g.:

      class C {
          static String s;
      }

      ...

              C o = new C();
              C.s = taint;
              sink(o.s); // finding ! (CODE-7871)
- No more RPC error when using --sarif with some join-mode rules.
  Moreover, regular rules without the 'languages:' field will be skipped
  instead of aborting the whole scan. (gh-10723)
