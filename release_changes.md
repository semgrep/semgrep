## [1.96.0](https://github.com/returntocorp/semgrep/releases/tag/v1.96.0) - 2024-11-07


### Added


- The pro engine now handles duplicate function names in C. When duplicate
  functions are found, we assume that any of the duplicated functions could be
  called. For example, if the function `foo` is defined in two different files,
  taint errors will be reported for both instances:

  ```
  // "a/test.h"
  void foo(int x) {
      //deepruleid: dup-symbols
      sink(x);
  }

  // "b/test.h"
  void foo(int x) {
      //deepruleid: dup-symbols
      sink(x);
  }

  // "main.c"
  #ifdef HEADER_A
      #include "a/test.h"
  #else
      #include "b/test.h"
  #endif

  int main() {
      int x = source();
      foo(x);
  }
  ``` (code-7654)


### Changed


- Reduced memory allocations while processing nosemgrep comments, improving memory use and time for scans with a large number of findings. (nosem-mem)


### Fixed


- Optimized taint-mode (only in Pro) to scale better when there is a large number
  of matches of sources/propagators/sanitizers/sinks within a function. (flow-83)
- Fixed a bug in the supply chain scanner's gradle lockfile parser. Previously, semgrep would fail to parse
  any gradle lockfile which did not start with a specific block comment. Now, semgrep will parse gradle
  lockfiles correctly by ignoring the comment (allowing any or no comment at all to exist). (gh-10508)
- Exceptions thrown during the processing of a target should not fail
  the whole scan anymore (regression introduced in 1.94.0). The scan will
  have an exit code of 0 instead of 2 (unless the user passed --strict in which
  case it will exit with code 2). (incid-110)
- Fix exponential parsing time with generic mode on input containing many
  unclosed braces on the same line. (saf-1667)
- Fix regexp parsing error occurring during ReDoS analysis when encountering
  a character class starting with `[:` such as `[:a-z]`. (saf-1693)
- Fix in `semgrep scan`: anchored semgrepignore patterns for folders such
  as `/tests` are now honored properly. Such patterns had previously no
  effect of target file filtering. (semgrepignore-anchored-dirs)
