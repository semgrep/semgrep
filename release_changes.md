## [1.114.0](https://github.com/semgrep/semgrep/releases/tag/v1.114.0) - 2025-03-19


### Fixed


- Pro Engine now more accurately tracks the scope of Python local variables. For
  example, the following code defines two `z` variables that should be tracked
  separately.

  ```
  z = 1

  def foo():
      z = 2
      a = z
  ```

  The Pro engine now correctly recognizes that the `z` assigned to `a` is the one
  defined in the local scope, not the global scope. (code-8114)
