# Semgrep Docs

Welcome to the extended Semgrep docs! 🙌 The Director's Cut, if you will.

## Language Support

For every language that Semgrep supports, there are certain features that must
be implemented, like support for the ellipsis (`...`) operator, metavariables, and more.

See [here](matrix.md) for a more detailed breakdown of feature support by
language.

## Semgrep Output

Semgrep by default prints to STDOUT, but it can also output JSON or SARIF (Static Analysis Results Interchange Format). See [here](output.md) for more details.

## Integrations

It's easy to integrate Semgrep as a pre-commit hook or in one of many
continuous integration (CI) systems. 

See [here](integrations.md) for example config files to run Semgrep using
tools like AppVeyor, CircleCI, TravisCI, GitHub Actions, and Gitlab.

## Writing Rules

In-depth docs:
* [Pattern Features](pattern-features.md) describes what Semgrep patterns can do
  in detail, and provides many example use cases of metavariables, the ellipsis
  operator, and more.
* Semgrep YAML rule files enable you to combine patterns in powerful ways,
  allowing you to find code patterns that do or don't match multiple sets of
  patterns. See the [config file spec](configuration-files.md) for more details.

You can write and share rules directly from the live editor at https://semgrep.live or run Semgrep via a standalone CLI or Docker.

### Semgrep Features

Semgrep patterns have a handful of primary features:

#### Metavariables

**Metavariables** are an abstraction that Semgrep provides when you want to
match something but you don't know exactly what it is ahead of time. You can
think of *metavariables* like a [capture
group](https://regexone.com/lesson/capturing_groups) in regular expressions.

Metavariable names look like `$X`, `$WIDGET`, or `$USERS_2`. They can only
contain uppercase characters, or `_`, or digits, and must start with an
uppercase character or `_`. Names like `$x` or `$some_value` are invalid.  

#### The `...` (ellipsis) operator 

The **ellipsis operator** abstracts away
sequences of zero or more arguments, statements, characters, [and more](pattern-features.md).

For example,
```yaml
$FILE = open(...)
```
will find all occurrences in your code where the result of an `open()` call with zero or more arguments is assigned
to a variable.

#### Composing Patterns

You can also construct rules by composing multiple patterns together.

Let's consider an example:

```yaml
rules:
  - id: open-never-closed
    patterns:
      - pattern: $FILE = open(...)
      - pattern-not-inside: |
          $FILE = open(...)
          ...
          $FILE.close()
    message: "file object opened without corresponding close"
    languages: [python]
    severity: ERROR
```

This rule looks for files that are opened but never closed. It accomplishes
this by looking for the `open(...)` pattern _and not_ a following `close()`
pattern. The `$FILE` metavariable ensures that the same variable name is used
in the `open` and `close` calls. The ellipsis operator allows for any arguments
to be passed to `open` and any sequence of code statements in-between the `open`
and `close` calls. We don't care how `open` is called or what happens up to
a `close` call, we just need to make sure `close` is called.

**For more information on rule fields like `patterns` and `pattern-not-inside`
see the [configuration documentation](docs/configuration-files.md).**

#### Equivalences

Equivalences are another key Semgrep concept. Semgrep automatically searches
for code that is semantically equivalent. 

For example, the following patterns
are semantically equivalent, and thus the pattern `subprocess.Popen(...)` will fire on both.

```python
subprocess.Popen("ls")
```

```python
from subprocess import Popen as sub_popen

result = sub_popen("ls")
```

For a full list of Semgrep feature support by language see the
[language matrix](docs/matrix.md).

## Contributing to Semgrep

We'd love your help! See [here](development.md) for detailed steps on getting your dev environment set up, building Semgrep, and more.