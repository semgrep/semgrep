# Experimental Features

## Autofix

Experimental support is available in Semgrep using the `fix:` key in rule configuration files.

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

See it live at https://semgrep.dev/R6g.

## Autofix Using Regular Expression Replacement

Semgrep can do regular expression replacement as an autofix. Use the `fix-regex` key to apply regular expression replacements to matches found by Semgrep.

`fix-regex` has two required fields, `regex` and `replacement`. `regex` specifies the regular expression to replace within the match found by Semgrep. `replacement` specifies what to replace the regular expression with. 

`fix-regex` also takes an optional `count` field, which specifies how many occurrences of `regex` to replace with `replacement`, from left-to-right and top-to-bottom. By default, `fix-regex` will replace all occurrences of `regex`. If `regex` does not match anything, no replacements are made.

The replacement behavior is identical to the `re.sub` function in Python. See https://docs.python.org/3/library/re.html#re.sub for more information.

An example rule with `fix-regex` is shown below. `regex` uses a capture group to greedily capture everything up to the final parenthesis in the match found by Semgrep. `replacement` replaces this with everything in the capture group (`\1`), a comma, `timeout=30`, and a closing parenthesis. Effectively, this adds `timeout=30` to the end of every match.

```yaml
rules:
- id: python.requests.best-practice.use-timeout.use-timeout
  patterns:
  - pattern-not: requests.$W(..., timeout=$N, ...)
  - pattern-not: requests.$W(..., **$KWARGS)
  - pattern-either:
    - pattern: requests.request(...)
    - pattern: requests.get(...)
    - pattern: requests.post(...)
    - pattern: requests.put(...)
    - pattern: requests.delete(...)
    - pattern: requests.head(...)
    - pattern: requests.patch(...)
  fix-regex:
    regex: '(.*)\)'
    replacement: '\1, timeout=30)'
  message: |
    'requests' calls default to waiting until the connection is closed.
    This means a 'requests' call without a timeout will hang the program
    if a response is never received. Consider setting a timeout for all
    'requests'.
  languages: [python]
  severity: WARNING
```

The autofix can be applied directly to the file using the `--autofix` flag, or you can use both the `--autofix` and `--dryrun` flags to test the autofix.

<p align="center">
  <img src="https://web-assets.r2c.dev/inline-autofix-regex.gif" width="100%" alt="Apply Semgrep autofix direclty to a file"/>
</p>

Please file issues with autofix [here](https://github.com/returntocorp/semgrep/issues) and include the `feature:autofix` tag.

## Equivalences

Experimental support for equivalences is available in Semgrep. Equivalences will define code patterns which Semgrep should consider equivalent.  For example, you may wish to define the commutative property for the plus operator:

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

See it live at https://semgrep.dev/AEL.

Please file issues with equivalences [here](https://github.com/returntocorp/semgrep/issues) and include the `feature:equivalences` tag.

## Taint Tracking

Python CLI support for taint tracking is now available! A taint-tracking rule uses the `mode: taint` key-value pair and replaces the typical [top-level pattern keys](https://github.com/returntocorp/semgrep/blob/develop/docs/configuration-files.md) with `pattern-sources` and `pattern-sinks` (required) and `pattern-sanitizers` (optional). For example:

```yaml
- id: rule_id
  mode: taint
  pattern-sources:
    - source(...)
    - source1(...)
  pattern-sinks:
    - sink(...)
    - sink1(...)
    - eval(...)
  pattern-sanitizers:
    - sanitize(...)
    - sanitize1(...)
  message: A user input source() went into a dangerous sink()
  languages: [python]
  severity: WARNING
```

A file containing the rule shown above can be found at `../semgrep-core/data/basic_tainting.yml`. To see this taint-tracking example in action, use the following command:

```yaml
semgrep --config ../semgrep-core/data/basic_tainting.yml ../semgrep-core/tests/TAINTING
```
