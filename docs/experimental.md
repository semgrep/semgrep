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

Please file issues with autofix [here](https://github.com/returntocorp/semgrep/issues) and include the `feature:equivalences` tag.

## Taint Tracking

Experimental support for taint tracking will be available in semgrep soon.
