# Experimantal Features

## Autofix

Experimental support is available in semgrep using the `fix:` key in rule configuration files.

Example rule with autofix:

```yaml
rules:
- id: use-sys-exit
  languages:
  - python
  message: |
    Use `sys.exit` over the python shell `exit` built-in. `exit` is a helper
    for the interactive shell and is not be available on all Python implementations.
    https://stackoverflow.com/a/6501134
  pattern: exit($X)
  fix: sys.exit($X)
  severity: WARNING
```

See it live at https://semgrep.live/R6g.


Please file issues with autofix [here](https://github.com/returntocorp/semgrep/issues) and include the `feature:autofix` tag.

## Equivalences

Experimental support for equivalences is available in semgrep. Equivalences will define code patterns which semgrep should consider equivalent.  For example, you may wish to define the commutative property for the plus operator:

```
$X + $Y <==> $Y + $X
```

To define equivalences, use the `equivalences:` top-level key and one `- equivalence:` key for each equivalence.

Example rule with equivalences:

```yaml
rules:
  - id: open-redirect
    languages: [python]
    equivalences:
      - equivalence: request.$W.get(...) ==> request.$W(...)
    patterns:
      - pattern-inside: |
          def $FUNC(...):
            ...
      - pattern-not-inside: |
          def $FUNC(...):
            ...
            django.utils.http.is_safe_url(...)
            ...
      - pattern-not-inside: |
          if <... django.utils.http.is_safe_url(...) ...>:
            ...
          ...
      - pattern-either:
        - pattern: django.shortcuts.redirect(..., request.$W.get(...), ...)
        - pattern: django.shortcuts.redirect(..., $S.format(..., request.$W.get(...), ...), ...)
        - pattern: django.shortcuts.redirect(..., $S % request.$W.get(...), ...)
        - pattern: django.shortcuts.redirect(..., f"...{request.$W.get(...)}...", ...)
    message: "Open redirect detected."
    severity: WARNING
```

See it live at https://semgrep.live/AEL.

Please file issues with equivalences [here](https://github.com/returntocorp/semgrep/issues) and include the `feature:equivalences` tag.

## Typed Metavariables

  Experimental support for typed metavariables is available for Java and Go in semgrep. Typed metavariables specify that the metavariable is only matched if it is of a specific type. For example, you may want to specifically check that == is never used for Strings:

  ```
 (String $X) == $Y
 ```

  In Java, you could do this with a rule such as

  ```yaml
 rules:
   - id: no-string-cmp
     languages: [java]
     patterns:
       - pattern: $X == (String $Y)
     message: "Strings should not be compared with =="
     severity: WARNING
 ```

  If we had the code

  ```java
 public class Example {
     public int foo(String a, int b) {
         //ERROR: this one is matched
         if (a == "hello") return 1;
         // This one is not
         if (b == 2) return -1;
     }
 }
 ```

  only the string comparison would result in an error.

  Similarly, in Go, we could use the rule

  ```yaml
 rules:
   - id: no-string-cmp
     languages: [go]
     patterns:
       - pattern: "$X == ($Y : str)"
     message: "Strings should not be compared with =="
     severity: WARNING
 ```

  though this check is less relevant in Go.

  See it live at https://semgrep.live/WADZ

  Limitations:

  Currently, since matching happens within a single file, this is only guaranteed to work for local variables and arguments. Additionally, it only understands types on the most shallow level. For example, if you have int[] A, it will not recognize A[0] as an integer. If you have a class with fields, you will not be able to use typechecking on field accesses, and it will not recognize the class's field as the expected type. Literal types are understood to a limited extent.

  Go currently does not recognize the type of all variables when declared on the same line. That is, if you have 

  ```go
 var a, b = 1
 ```

  it will not take both a and b as ints.

  Please file issues with type matching [here](https://github.com/returntocorp/semgrep/issues) and include the `feature:typematching` tag.

## Taint Tracking

Experimental support for taint tracking will be available in semgrep soon.
