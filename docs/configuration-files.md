# Configuration Files

This document describes `semgrep` configuration files and provides rule
examples. Configuration files are specified with the `--config` (or `-f`) flag.
A single [YAML](https://en.wikipedia.org/wiki/YAML) file or a directory of
files ending in `.yml` or `.yaml` may be specified. Each configuration file
must match the [schema](configuration-files.md#schema).

*For more information on the `--config` flag see [other configuration options](configuration-files.md#other-configuration-options).*

Contents:

* [Simple Example](configuration-files.md#simple-example)
* [Other Configuration Options](configuration-files.md#other-configuration-options)
* [Schema](configuration-files.md#schema)
* [Operators](configuration-files.md#operators)
  * [`pattern`](configuration-files.md#pattern)
  * [`patterns`](configuration-files.md#patterns)
  * [`pattern-either`](configuration-files.md#pattern-either)
  * [`pattern-regex`](configuration-files.md#pattern-regex)
  * [`metavariable-regex`](configuration-files.md#metavariable-regex)
  * [`metavariable-comparison`](configuration-files.md#metavariable-comparison)
  * [`pattern-not`](configuration-files.md#pattern-not)
  * [`pattern-inside`](configuration-files.md#pattern-inside)
  * [`pattern-not-inside`](configuration-files.md#pattern-not-inside)
  * [`pattern-where-python`](configuration-files.md#pattern-where-python)
* [Metavariable Matching](configuration-files.md#metavariable-matching)
  * [Metavariables in Logical ANDs](configuration-files.md#metavariables-in-logical-ands)
  * [Metavariables in Logical ORs](configuration-files.md#metavariables-in-logical-ors)
  * [Metavariables in Complex Logic](configuration-files.md#metavariables-in-complex-logic)
* [Optional Fields](configuration-files.md#optional-fields)
  * [`fix`](configuration-files.md#fix)
  * [`metadata`](configuration-files.md#metadata)
  * [`paths`](configuration-files.md#paths)
* [Other Examples](configuration-files.md#other-examples)
  * [Complete Useless Comparison](configuration-files.md#complete-useless-comparison)
* [Ignoring Findings](configuration-files.md#ignoring-findings)
* [Full Specification](configuration-files.md#full-specification)

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

Running `semgrep` against this code produces the following results:

```text
rule:eqeq-always-true: node.id == node.id is always true
3: if node.id == node.id:  # Oops, supposed to be 'node_id'
```

This should give you a basic idea of what the rule fields do.

## Other Configuration Options

`semgrep` configuration can be specified in a number of ways:

* A single YAML rule file, e.g. `--config /path/to/rules.yaml`, defaults to `.semgrep.yml`
* A directory of YAML rule files ending in `.yml` or `.yaml`, e.g. `--config /path/to/rules`
* A configuration URL, e.g. `--config https://rules.corp.com/semgrep.yml`
* A registry name, e.g. `--config r2c.python`

r2c provides a registry of curated rules, see [semgrep-rules](https://github.com/returntocorp/semgrep-rules)
for more information.

## Schema

**Required:**

All required fields must be present at the top-level of a rule. I.e. immediately underneath `rules`.

| Field | Type | Description |
| :--- | :--- | :--- |
| `id` | `string` | Unique, descriptive identifier . e.g. `no-unused-variable`. |
| `message` | `string` | Message highlighting why this rule fired and how to remediate the issue. |
| `severity` | `string` | One of: `WARNING`, `ERROR`. |
| `languages` | `array` | Any of: `c`, `go`, `java`, `javascript`, `python`, or `ruby`. |
| [`pattern`](configuration-files.md#pattern)_\*_ | `string` | Find code matching this expression. |
| [`patterns`](configuration-files.md#patterns)_\*_ | `array` | Logical AND of multiple patterns. |
| [`pattern-either`](configuration-files.md#pattern-either)_\*_ | `array` | Logical OR of multiple patterns. |
| [`pattern-regex`](configuration-files.md#pattern-regex)_\*_ | `string` | Search files for [Python `re`](https://docs.python.org/3/library/re.html) compatible expressions. |

_\* Only one of `pattern`, `patterns`, `pattern-either`, or `pattern-regex` is required._

**Optional:**

| Field | Type | Description |
| :--- | :--- | :--- |
| [`fix`](configuration-files.md#fix) | `object` | Simple search-and-replace autofix functionality. |
| [`metadata`](configuration-files.md#metadata) | `object` | Arbitrary user-provided data. Attach data to rules without affecting semgrep's behavior. |
| [`paths`](configuration-files.md#paths) | `object` | Paths to include or exclude when running this check. |

The below optional fields must reside underneath a `patterns` or `pattern-either` field.

| Field | Type | Description |
| :--- | :--- | :--- |
| [`metavariable-regex`](configuration-files.md#metavariable-regex) | `map` | Filter on metavariable content and [Python `re.match`](https://docs.python.org/3/library/re.html#re.match) expressions. |
| [`metavariable-comparison`](configuration-files.md#metavariable-comparison) | `map` | Filter on metavariable content and basic [Python comparison](https://docs.python.org/3/reference/expressions.html#comparisons) expressions. |
| [`pattern-not`](configuration-files.md#pattern-not) | `string` | Logical NOT - remove findings matching this expression. |
| [`pattern-inside`](configuration-files.md#pattern-inside) | `string` | Keep findings that lie inside this pattern. |
| [`pattern-not-inside`](configuration-files.md#pattern-not-inside) | `string` | Keep findings that do not lie inside this pattern. |
| [`pattern-where-python`](configuration-files.md#pattern-where-python) | `string` | Remove findings matching this Python expression. |

## Operators

### `pattern`

The `pattern` operator looks for code matching its expression. As noted above, this can be basic expressions like `$X == $X` or blacklisted functionality like `crypto.md5(...)`.

**Example**

See the [Simple Example](configuration-files.md#simple-example) above.

### `patterns`

The `patterns` operator performs a logical AND operation on one or more child patterns. This is useful for chaining multiple patterns together that all must be true.

**Example**

Let's build on the [Simple Example](configuration-files.md#simple-example) above:

```yaml
rules:
  - id: eqeq-always-true
    patterns:
      - pattern: $X == $X
      - pattern-not: 0 == 0
    message: "$X == $X is always true"
    languages: [python]
    severity: ERROR
```

Checking if `0 == 0` is often used to quickly enable and disable blocks of code. It can easily be changed to `0 == 1` to disable functionality. We can remove these debugging false positives with `patterns`.

### `pattern-either`

The `pattern-either` operator performs a logical OR operation on one or more child patterns. This is useful for chaining multiple patterns together where any may be true.

**Example**

```yaml
rules:
  - id: insecure-crypto-usage
    pattern-either:
      - pattern: hashlib.md5(...)
      - pattern: hashlib.sha1(...)
    message: "insecure cryptography hashing function"
    languages: [python]
    severity: ERROR
```

This rule looks for usage of the Python standard library functions `hashlib.md5` **or** `hashlib.sha1`. Depending on their usage, these hashing functions are [considered insecure](https://shattered.io/).

### `pattern-regex`

The `pattern-regex` operator searches files for a [Python `re`](https://docs.python.org/3/library/re.html) compatible expression. This is useful for migrating existing regular expression code search functionality to `semgrep`.

**Example**

The `pattern-regex` operator can be combined with other pattern operators:

```yaml
rules:
  - id: boto-client-ip
    patterns:
      - pattern-inside: boto3.client(host="...")
      - pattern-regex: '\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}'
    message: "boto client using IP address"
    languages: [python]
    severity: ERROR
```

It can also be used as a standalone, top-level operator:

```yaml
rules:
  - id: legacy-eval-search
    pattern-regex: 'eval\('
    message: "insecure code execution"
    languages: [javascript]
    severity: ERROR
```

*Note that single (`'`) and double (`"`) quotes [behave differently](https://docs.octoprint.org/en/master/configuration/yaml.html#scalars)
in YAML syntax. Single quotes are typically preferred when using backslashes (`\`) with `pattern-regex`.*

### `metavariable-regex`

The `metavariable-regex` operator searches metavariables for a [Python `re.match`](https://docs.python.org/3/library/re.html#re.match)
compatible expression. This is useful for filtering results based on a [metavariable's](/docs/pattern-features.md#metavariables)
value.

**Example**

The `metavariable-regex` operator is a mapping which requires the
`metavariable` and `regex` keys. It can be combined with other pattern operators:

```yaml
rules:
  - id: insecure-methods
    patterns:
      - pattern: module.$METHOD(...)
      - metavariable-regex:
          metavariable: '$METHOD'
          regex: '(insecure1|insecure2|insecure3)'
    message: "module using insecure method call"
    languages: [python]
    severity: ERROR
```

### `metavariable-comparison`

The `metavariable-comparison` operator compares metavariables against a basic [Python comparison](https://docs.python.org/3/reference/expressions.html#comparisons)
expression. This is useful for filtering results based on a [metavariable's](/docs/pattern-features.md#metavariables) numeric value.

**Example**

The `metavariable-comparison` operator is a mapping which requires the
`metavariable` and `comparison` keys. It can be combined with other pattern operators:

```yaml
rules:
  - id: superuser-port
    patterns:
      - pattern: set_port($ARG)
      - metavariable-comparison:
          metavariable: '$ARG'
          comparison: '$ARG < 1024'
    message: "module setting superuser port"
    languages: [python]
    severity: ERROR
```

This will catch code like `set_port(80)` or `set_port(443)`, but not `set_port(8080)`.

The `metavariable-comparison` operator also takes optional `base: int` and
`strip: bool` keys. These keys set the integer base the metavariable value
should be interpreted as and remove quotes from the metavariable value,
respectively.

For example, `base`:

```
- pattern: set_permissions($ARG)
- metavariable-comparison:
    metavariable: '$ARG'
    comparison: '$ARG > 0o600'
    base: 8
```

This will interpret metavariable values found in code as octal, so `0700`
will be detected, but `0500` will not.

For example, `strip`:

```
- pattern: to_integer($ARG)
- metavariable-comparison:
    metavariable: '$ARG'
    comparison: '$ARG > 2147483647'
    strip: true
```

This will remove quotes (`'`, `"`, and `` ` ``) from both ends of the
metavariable content. So `"2147483648"` will be detected but `"2147483646"`
will not. This is useful when you expect strings to contain integer or float
data.

### `pattern-not`

The `pattern-not` operator is the opposite of the `pattern` operator. That is, it finds code that does not match its expression. This is useful for eliminating common false positives.

**Example**

See the [`patterns`](configuration-files.md#patterns) example above.

### `pattern-inside`

The `pattern-inside` operator keeps matched findings that reside within its expression. This is useful for finding code inside other pieces of code like functions or if blocks.

**Example**

```yaml
rules:
  - id: return-in-init
    patterns:
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

The `pattern-not-inside` operator keeps matched findings that do not reside within its expression. It is the opposite of `pattern-inside`. This is useful for finding code that's missing a corresponding cleanup action like disconnect, close, or shutdown. It's also useful for finding problematic code that isn't inside code that mitigates the issue.

**Example**

```yaml
rules:
  - id: open-never-closed
    patterns:
      - pattern: $F = open(...)
      - pattern-not-inside: |
          $F = open(...)
          ...
          $F.close()
    message: "file object opened without corresponding close"
    languages: [python]
    severity: ERROR
```

This rule looks for files that are opened but never closed, which can lead to
resource exhaustion. It accomplishes this by looking for the `open(...)` pattern
_and not_ a following `close()` pattern.

The `$FILE` metavariable ensures that the same variable name is used in the
`open` and `close` calls. The ellipsis operator allows for any arguments to be
passed to `open` and any sequence of code statements in-between the `open` and
`close` calls. We don't care how `open` is called or what happens up to a
`close` call, we just need to make sure `close` is called.

### `pattern-where-python`

The `pattern-where-python` is the most flexible operator. It allows for writing custom Python logic to filter findings. This is useful when none of the other operators provide the functionality needed to create a rule.

**This operator must also be used with caution. Its use allows for arbitrary Python code execution. As a defensive measure, the `--dangerously-allow-arbitrary-code-execution-from-rules` flag must also be enabled to use `pattern-where-python` rules.**

**Example**

```yaml
rules:
  - id: use-decimalfield-for-money
    patterns:
      - pattern: $FIELD = django.db.models.FloatField(...)
      - pattern-inside: |
          class $CLASS(...):
              ...
      - pattern-where-python: "'price' in vars['$FIELD'] or 'salary' in vars['$FIELD']"
    message: "use DecimalField for currency fields to avoid float-rounding errors"
    languages: [python]
    severity: ERROR
```

This rule looks for usage of Django's [`FloatField`](https://docs.djangoproject.com/en/3.0/ref/models/fields/#django.db.models.FloatField) model when storing currency information. `FloatField` can lead to rounding errors and should be avoided in favor of [`DecimalField`](https://docs.djangoproject.com/en/3.0/ref/models/fields/#django.db.models.DecimalField) when dealing with currency. Here the `pattern-where-python` operator allows us to utilize the Python `in` statement to filter findings that look like currency.

## Metavariable Matching

Metavariable matching operates differently for logical AND (`patterns`)
and logical OR (`pattern-either`) parent operators. Note that the behavior is
consistent across all child operators: `pattern`, `pattern-not`,
`pattern-regex`, `pattern-inside`, `pattern-not-inside`.

### Metavariables in Logical ANDs

Metavariable values must be identical across sub-patterns when performing
logical AND operations with the `patterns` operator.

**Example**

Consider the following rule:

```yaml
rules:
  - id: function-args-to-open
    patterns:
      - pattern-inside: |
          def $F($X):
              ...
      - pattern: open($X)
    message: "Function argument passed to open() builtin"
    languages: [python]
    severity: ERROR
```

This rule will match the following code:

```python
def foo(path):
    open(path)
```

But will not match this code:

```python
def foo(path):
    open(something_else)
```

### Metavariables in Logical ORs

Metavariable matching does not affect the matching of logical OR operations
with the `pattern-either` operator.

**Example**

Consider the following rule:

```yaml
rules:
  - id: insecure-function-call
    pattern-either:
      - pattern: insecure_func1($X)
      - pattern: insecure_func2($X)
    message: "Insecure function use"
    languages: [python]
    severity: ERROR
```

This rule will match both examples below:

```python
insecure_func1(something)
insecure_func2(something)
```

```python
insecure_func1(something)
insecure_func2(something_else)
```

### Metavariables in Complex Logic

Metavariable matching still affects subsequent logical ORs if the parent is a
logical AND.

**Example**

Consider the following rule:

```yaml
  patterns:
    - pattern-inside: |
        def $F($X):
          ...
    - pattern-either:
        - pattern: bar($X)
        - pattern: baz($X)
```

This rule matches both examples below:

```python
def foo(something):
    bar(something)
```

```python
def foo(something):
    baz(something)
```

But will not match this code::

```python
def foo(something):
    bar(something_else)
```

## Optional Fields

### `fix`

The `fix` top-level key allows for simple autofixing of a pattern by suggesting an autofix for each match. Run `semgrep` with `--autofix` to apply the changes to the files.

**Example**

```yaml
rules:
  - id: use-dict-get
    patterns:
      - pattern: $DICT[$KEY]
    fix: $DICT.get($KEY)
    message: "Use `.get()` method to avoid a KeyNotFound error"
    languages: [python]
    severity: ERROR
```

### `metadata`

In some cases, you might wish to note some extra information on a rule, such as a CVE that was created for it,
or the name of the security engineer that came up with the rule.

You can use the `metadata:` key in these cases, like so:

```yaml
rules:
  - id: eqeq-is-bad
    patterns:
      - [...]
    message: "useless comparison operation `$X == $X` or `$X != $X`"
    metadata:
      cve: CVE-2077-1234
      discovered-by: Ikwa L'equale
```

The metadata will also be reproduced in semgrep's output if you're running it with `--json`.

### `paths`

#### Excluding a Rule in Paths

To ignore a specific rule on specific files, set a `paths:` key with one or more filters like so:

```yaml
rules:
  - id: eqeq-is-bad
    pattern: $X == $X
    paths:
      exclude:
        - "*.jinja2"
        - "*_test.go"
        - "project/tests"
        - project/static/*.js
```

When invoked with `semgrep -f rule.yaml project/`,
this rule will run on files inside `project/`,
but no results will be returned for:

- any file with a `.jinja2` file extension
- any file whose name ends in `_test.go`, such as `project/backend/server_test.go`
- any file inside `project/tests` or its subdirectories
- any file matching the `project/static/*.js` glob pattern

**Note:** the glob syntax is [the one in Python's `pathlib`](https://docs.python.org/3/library/pathlib.html#pathlib.PurePath.match),
which is used to match against the given file and all its parent directories.

#### Limiting a Rule to Paths

Conversely, to run a rule *only* on specific files, set a `paths:` key with one or more of these filters:

```yaml
rules:
  - id: eqeq-is-bad
    pattern: $X == $X
    paths:
      include:
        - "*_test.go"
        - "project/server"
        - "project/schemata"
        - "project/static/*.js"
```

When invoked with `semgrep -f rule.yaml project/`,
this rule will run on files inside `project/`,
but results will be returned only for:

- files whose name ends in `_test.go`, such as `project/backend/server_test.go`
- files inside `project/server`, `project/schemata`, or their subdirectories
- files matching the `project/static/*.js` glob pattern

**Note:** when mixing inclusion and exclusion filters, the exclusion ones take precedence.

For example a rule that specifies these paths:

```yaml
paths:
  include: "project/schemata"
  exclude: "*_internal.py"
```

Would return results from `project/schemata/scan.py`, but not from `project/schemata/scan_internal.py`.

## Other Examples

This section highlights more complex rules that perform advanced code searching.

### Complete Useless Comparison

```yaml
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
    message: "useless comparison operation `$X == $X` or `$X != $X`"
```

This rule makes use of many of the operators above. It uses `pattern-either`, `patterns`, `pattern`, and `pattern-inside` to carefully consider different cases, and uses `pattern-not-inside` and `pattern-not` to whitelist certain useless comparisons.

## Ignoring Findings

`semgrep` allows for ignoring, or whitelisting, findings inline in code. This
is achieved by specifying a `nosem` comment on the line of the finding. For
example, consider the following rule:

```yaml
rules:
  - id: bad-func-is-insecure
    pattern: bad_func(...)
    message: "bad_func usage is insecure"
    languages: [javascript]
    severity: ERROR
```

We can ignore specific findings of this rule with the following comments:

```javascript
bad_func() // nosem
bad_func() // nosem: bad-func-is-insecure
```

A naked `nosem` comment will ignore all `semgrep` findings on this line. A
`nosem` comment specifying a specific rule ID will only ignore that rule.
Multiple rules can be ignored using a comma-delimited list like:

```javascript
func() // nosem: pattern-id1, pattern-id2
```

Note that `nosem` comments must appear on the first line of a finding. This
means the following will not work:

```javascript
bad_func(
    arg // nosem: bad-func-is-insecure
)
```

`nosem` functionality works across languages. For example, the following code
will work just as well for Python:

```python
bad_func()  # nosem: bad-func-is-insecure
```

## Full specification

The [full configuration-file format](/semgrep/semgrep/rule_schema.yaml) is defined as
a [jsonschema](http://json-schema.org/specification.html) object.
