# Configuration Format

This document describes `sgrep` rule fields and provides rule examples.

Contents:

* [Simple Example](#simple-example)
* [Schema](#schema)
* [Operators](#operators)
  * [`pattern`](#pattern)
  * [`patterns-and`](#patterns-and)
  * [`patterns-or`](#patterns-or)
  * [`pattern-not`](#pattern-not)
  * [`pattern-inside`](#pattern-inside)
  * [`pattern-not-inside`](#pattern-not-inside)
  * [`pattern-where-python`](#pattern-where-python)
* [Other Examples](#other-examples)
  * [Complete Useless Comparison](#complete-useless-comparison)

## Simple Example

To get a feel for the rule format, let's start with a simple example:

```yaml
rules:
  - id: eqeq-always-true
    pattern: $X == $X
    message: "$X == $X is always true"
    languages: [python]
    severity: ERROR
```

This rule is useful for basic comparison mistakes. Consider the following:

```python
def get_node(node_id, nodes):
    for node in nodes:
        if node.id == node.id:  # Oops, supposed to be 'node_id'
            return node
    return None
```

Running `sgrep` against this code produces the following results:

```
rule:eqeq-always-true: node.id == node.id is always true
3: if node.id == node.id:  # Oops, supposed to be 'node_id'
```

This should give you a basic idea of what the rule fields do.

## Schema

**Required:**

All required fields must be present at the top-level of a rule. I.e.
immediately underneath `rules`.

| Field | Type | Description |
| :--- | :--- | :--- |
| `id` | `string` | Unique, descriptive identifier . e.g. `no-unused-variable`. |
| `message` | `string` | Message highlighting why this rule fired and how to remediate the issue. |
| `severity` | `string` | One of: `WARNING`, `ERROR`. |
| `languages` | `array` | Any of: `python`, `javascript`, or `go`. |
| [`pattern`](#pattern)_*_ | `string` | Find code matching this expression. |
| [`patterns-and`](#patterns-and)_*_ | `array` | Logical AND of multiple patterns. |
| [`patterns-or`](#patterns-or)_*_ | `array` | Logical OR of multiple patterns. |

_* Only one of `pattern`, `patterns-and`, or `patterns-or` is required._

**Optional:**

All optional fields must reside underneath a `patterns-and` or `patterns-or` field.

| Field | Type | Description |
| :--- | :--- | :--- |
| [`pattern-not`](#pattern-not) | `string` | Logical NOT - remove findings matching this expression. |
| [`pattern-inside`](#pattern-inside) | `string` | Keep findings that lie inside this pattern. |
| [`pattern-not-inside`](#pattern-not-inside) | `string` | Keep findings that do not lie inside this pattern. |
| [`pattern-where-python`](#pattern-where-python) | `string` | Remove findings matching this Python expression. |

## Operators

### `pattern`

The `pattern` operator looks for code matching its expression. As noted above,
this can be basic expressions like `$X == $X` or blacklisted functionality like
`crypto.md5(...)`.

**Example**

See the [Simple Example](#simple-example) above.

### `patterns-and`

The `patterns-and` operator performs a logical AND operation on one or more
child patterns. This is useful for chaining multiple patterns together that
all must be true.

**Example**

Let's build on the [Simple Example](#simple-example) above:

```yaml
rules:
  - id: eqeq-always-true
    patterns-and:
      - pattern: $X == $X
      - pattern-not: 0 == 0
    message: "$X == $X is always true"
    languages: [python]
    severity: ERROR
```

Checking if `0 == 0` is often used to quickly enable and disable blocks of
code. It can easily be changed to `0 == 1` to disable functionality.  We can
remove these debugging false positives with `patterns-and`.

### `patterns-or`

The `patterns-or` operator performs a logical OR operation on one or more
child patterns. This is useful for chaining multiple patterns together where
any may be true.

**Example**

```yaml
rules:
  - id: insecure-crypto-usage
    patterns-or:
      - pattern: hashlib.md5(...)
      - pattern: hashlib.sha1(...)
    message: "insecure cryptography hashing function"
    languages: [python]
    severity: ERROR
```

This rule looks for usage of the Python standard library functions `hashlib.md5`
**or** `hashlib.sha1`. Depending on their usage, these hashing functions are
[considered insecure](https://shattered.io/).

### `pattern-not`

The `pattern-not` operator is the opposite of the `pattern` operator. That is,
it finds code that does not match its expression. This is useful for eliminating
common false positives.

**Example**

See the [`patterns-and`](#patterns-and) example above.

### `pattern-inside`

The `pattern-inside` operator keeps matched findings that reside within its
expression. This is useful for finding code inside other pieces of code like
functions or if blocks.

**Example**

```yaml
rules:
  - id: return-in-init
    patterns-and:
      - pattern: return ...
      - pattern-inside: |
          class $CLASS(...):
              ...
              def __init__(...):
                  ...
    message: "return should never appear inside a class __init__ function"
    languages: [python]
    severity: ERROR
```

Which fires on the following code:

```python
class Cls(object):
    def __init__(self):
        return None
```

### `pattern-not-inside`

The `pattern-not-inside` operator keeps matched findings that do not reside
within its expression. It is the opposite of `pattern-inside`. This is useful
for finding code that's missing a corresponding cleanup action like disconnect,
close, or shutdown. It's also useful for finding problematic code that isn't
inside code that mitigates the issue.

**Example**

```yaml
rules:
  - id: open-never-closed
    patterns-and:
      - pattern: $F = open(...)
      - pattern-not-inside: |
          $F = open(...)
          ...
          $F.close()
    message: "file object opened without corresponding close"
    languages: [python]
    severity: ERROR
```

This rule looks for calls to the Python `open` function without a corresponding
`close`. Without a `close` call the file will be left open which can lead to
resource exhaustion.

### `pattern-where-python`

The `pattern-where-python` is the most flexible operator. It allows for writing
custom Python logic to filter findings. This is useful when none of the other
operators provide the functionality needed to create a rule.

**This operator must also be used with caution. Its use allows for arbitrary
Python code execution.  As a defensive measure, the `--dangerously-allow-arbitrary-code-execution-from-rules`
flag must also be enabled to use `pattern-where-python` rules.**

**Example**

```yaml
rules:
  - id: use-decimalfield-for-money
    patterns-and:
      - pattern: $FIELD = django.db.models.FloatField(...)
      - pattern-inside: |
          class $CLASS(...):
              ...
      - pattern-where-python: "'price' in vars['$FIELD'] or 'salary' in vars['$FIELD']"
    message: "use DecimalField for currency fields to avoid float-rounding errors"
    languages: [python]
    severity: ERROR
```

This rule looks for usage of Django's [`FloatField`](https://docs.djangoproject.com/en/3.0/ref/models/fields/#django.db.models.FloatField)
model when storing currency information. `FloatField` can lead to rounding
errors and should be avoided in favor of [`DecimalField`](https://docs.djangoproject.com/en/3.0/ref/models/fields/#django.db.models.DecimalField)
when dealing with currency. Here the `pattern-where-python` operator allows us
to utilize the Python `in` statement to filter findings that look like
currency.

## Other Examples

This section highlights more complex rules that perform advanced code searching.

### Complete Useless Comparison

```yaml
rules:
  - id: eqeq-is-bad
    patterns-and:
      - pattern-not-inside: |
          def __eq__(...):
              ...
      - pattern-not-inside: assert(...)
      - pattern-not-inside: assertTrue(...)
      - pattern-not-inside: assertFalse(...)
      - patterns-or:
          - pattern: $X == $X
          - pattern: $X != $X
          - patterns-and:
            - pattern-inside: |
                 def __init__(...):
                      ...
            - pattern: self.$X == self.$X
      - pattern-not: 1 == 1
    message: "useless comparison operation `$X == $X` or `$X != $X`"
```

This rule makes use of many of the operators above. It uses `patterns-or`,
`patterns-and`, `pattern`, and `pattern-inside` to carefully consider
different cases, and uses `pattern-not-inside` and `pattern-not` to whitelist
certain useless comparisons.
