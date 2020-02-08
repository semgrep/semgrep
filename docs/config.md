## Config format

This document provides examples for the .yaml format for specifying rules to sgrep. It does not cover the patterns themselves; for information on the query sytnax see [patterns.md](patterns.md)

If you want run these rules or ship them to your team, rather than write them, check out the easy-to-use [bento.dev](https://bento.dev)

## Simple Example

```yaml
rules:
  - id: eqeq-is-bad
    pattern: $X == $X
    message: "Dude, $X == $X is always true!"
    languages: [python]
    severity: ERROR
```

This is a minimal rule. It will be run on any Python code, and will output the rule `id` "eqeq-is-bad" along with the `message` and the line and column range the match occurred on. So it will match code like `1 == 1` and output the message: "Dude, 1 == 1 is always true!"

## Complex Example
```
rules:
  - id: eqeq-is-bad
    patterns:
      - pattern-not-inside: |
          def __eq__(...):
              ...
      - pattern-not-inside: assert(...)
      - pattern-not-inside: assertTrue(...)
      - pattern-not-inside: assertFalse(...)
      - pattern-either:
          - pattern: $X == $X
          - pattern: $X != $X
          - patterns:
            - pattern-inside: |
                 def __init__(...):
                      ...
            - pattern: self.$X == self.$X
      - pattern-not: 1 == 1
    message: "useless comparison operation `$X == $X` or `$X != $X`; if testing for floating point NaN, use `math.isnan`, or `cmath.isnan` if the number is complex."
    languages: [python]
    severity: ERROR
```

The main difference here is that we are composing patterns together. Every pattern is implicitly ANDed together, i.e.:

```
    ...
    patterns:
      - pattern: foo()
      - pattern: foo(1)
```

Will match nothing, because foo() and foo(1) can never occur together. If we made the first pattern `foo(...)`, the rule will work, although the pattern is unecessary.

There are several operators that can be used under `patterns`:

- `pattern`: The rule will only fire if this pattern is found.
- `pattern-not`: Opposite of `pattern`
- `pattern-either`: You can put multiple other patterns under this; any of those patterns will count as a match.
- `pattterns`: Like `pattern-either`, you can put multiple patterns under this to create nested, implicitly-ANDed instructions.
- `pattern-inside`: The rule will only fire if the following patterns are inside this specified pattern. Useful for specifying a function that this behavior must occur in, for instance.
- `pattern-not-inside`: Opposite of `pattern-inside`

## Schema

Each rule object has these fields:

| Field     | Type          | Description                                                                                                        | Required |
| --------- | ------------- | ------------------------------------------------------------------------------------------------------------------ | -------- |
| id        | string        | None unique check-id that should be descriptive and understandable in isolation by the user. e.g. `no-unused-var`. | Y        |
| `pattern` or `patterns`   | string        | See example patterns in this document.                                                                                        | Y        |
| message   | string        | Description of the rule that will be output when a match is found.                                                 | Y        |
| languages | array<string> | Languages the check is relevant for. Can be python or javascript.                                                  | Y        |
| severity  | string        | Case sensitive string equal to WARNING, ERROR                                                                  | Y        |
