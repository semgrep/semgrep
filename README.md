# Semgrep

![Homebrew](https://github.com/returntocorp/homebrew-semgrep/workflows/homebrew/badge.svg)
[![r2c Community Slack](https://img.shields.io/badge/r2c_slack-join-brightgreen?style=flat&logo=slack&labelColor=4A154B)](https://join.slack.com/t/r2c-community/shared_invite/enQtNjU0NDYzMjAwODY4LWE3NTg1MGNhYTAwMzk5ZGRhMjQ2MzVhNGJiZjI1ZWQ0NjQ2YWI4ZGY3OGViMGJjNzA4ODQ3MjEzOWExNjZlNTA)

`semgrep` is a tool for easily detecting and preventing bugs and anti-patterns in
your codebase. It combines the convenience of `grep` with the correctness of
syntactical and semantic search. Developers, DevOps engineers, and security engineers
use `semgrep` to write code with confidence.

**Try it now:** [https://semgrep.live](https://semgrep.live/)

## Overview

Language support:

| **Python** | **Javascript** | **Go &nbsp; &nbsp; &nbsp;** | **Java &nbsp;** | **C &nbsp; &nbsp; &nbsp; &nbsp;** | **Typescript** | **PHP &nbsp; &nbsp;** |
|:-----------|:---------------|:----------------------------|:----------------|:----------------------------------|:---------------|:----------------------|
| ✅          | ✅              | ✅                           | ✅               | ✅                                 | Coming...      | Coming...             |

Example patterns:

| **Pattern**                                                        | **Matches**                                                |
|:-------------------------------------------------------------------|:-----------------------------------------------------------|
| [`$X == $X`](https://semgrep.live/20B)                             | `if (node.id == node.id): ...`                             |
| [`requests.get(..., verify=False, ...)`](https://semgrep.live/jqn) | `requests.get(url, timeout=3, verify=False)`               |
| [`os.system(...)`](https://semgrep.live/1W5)                       | `from os import system; system('echo semgrep')`            |
| [`$ELEMENT.innerHTML`](https://semgrep.live/9ze)                   | ``el.innerHTML = "<img src='x' onerror='alert(`XSS`)'>";`` |
| [`$TOKEN.SignedString([]byte("..."))`](https://semgrep.live/rXW)   | `ss, err := token.SignedString([]byte("HARDCODED KEY"))`   |

→ [see more example patterns in the live registry viewer](https://semgrep.live/registry)

## Installation

On macOS, binaries are available via [Homebrew](https://formulae.brew.sh/formula/semgrep):

```bash
brew install returntocorp/semgrep/semgrep
```

On Ubuntu, an install script is available on each release [here](https://github.com/returntocorp/semgrep/releases/download/v0.8.0/semgrep-v0.8.0-ubuntu-generic.sh)
```bash
./semgrep-v0.8.0-ubuntu-generic.sh
```

To try `semgrep` without installation, you can also run it via [Docker](https://docs.docker.com/install/):

```
docker run --rm -v "${PWD}:/home/repo" returntocorp/semgrep --help
```

## Usage

### Example Usage

Here is a simple Python example, `test.py`. We want to retrieve an object by ID:

```python3
def get_node(node_id, nodes):
    for node in nodes:
        if node.id == node.id:  # Oops, supposed to be 'node_id'
            return node
    return None
```

This is a bug. Let's use `semgrep` to find bugs like it, using a simple search pattern: `$X == $X`. It will find all places in our code where the left- and right-hand sides of a comparison are the same expression:

```
$ semgrep --lang python --pattern '$X == $X' test.py
test.py
3:        if node.id == node.id:  # Oops, supposed to be 'node_id'
```

## Configuration

For simple patterns use the `--lang` and `--pattern` flags. This mode of
operation is useful for quickly iterating on a pattern on a single file or
folder:

```bash
semgrep --lang javascript --pattern 'eval(...)' path/to/file.js
```

### Configuration Files

For advanced configuration use the `--config` flag. This flag automagically
handles a multitude of input configuration types:

* `--config <file|folder|yaml_url|tarball_url|registy_name>`

In the absence of this flag, a default configuration is loaded from `.semgrep.yml`
or multiple files matching `.semgrep/**/*.yml`.

#### Pattern Features

`semgrep` patterns make use of two primary features:

* **Metavariables like `$X`, `$WIDGET`, or `$USERS_2`.** Metavariable names can
only contain uppercase characters, or `_`, or digits, and must start with
an uppercase character or `_` - names like `$x` or `$some_value` are
invalid.  Metavariables are used to track a variable across a specific code
scope.
* **The `...` (ellipsis) operator.** The ellipsis operator abstracts away
sequences so you don't have to sweat the details of a particular code pattern.

For example,
```yaml
$FILE = open(...)
```
will find all occurrences in your code where the result of an `open()` call is assigned
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

Equivalences are another key concept in `semgrep`. `semgrep` automatically searches
for code that is semantically equivalent. For example, the following patterns
are semantically equivalent. The pattern `subprocess.Popen(...)` will fire on both.

```python
subprocess.Popen("ls")
```

```python
from subprocess import Popen as sub_popen

result = sub_popen("ls")
```

For a full list of `semgrep` feature support by language see the
[language matrix](docs/matrix.md).

### Registry

As mentioned above, you may also specify a registry name as configuration.
[r2c](https://r2c.dev) provides a [registry](https://github.com/returntocorp/semgrep-rules)
of configuration files. These rules have been tuned on thousands of repositories
using our [analysis platform](https://app.r2c.dev).

```bash
semgrep --config r2c
```

### Programmatic Usage

To integrate semgrep's results with other tools,
you can get results in machine-readable JSON format with the `--json` option,
or formatted according to the
[SARIF standard](https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html)
with the `--sarif` flag.

See our [output documentation](docs/output.md) for details.

## Resources

* [`semgrep` presentation at HellaSecure](https://www.youtube.com/watch?v=M586wePrwYs) and [slides](https://bit.ly/hella-secure-semgrep)
* [Pattern features documentation](docs/pattern-features.md)
* [Configuration files documentation](docs/configuration-files.md)
* [Integrations](docs/integrations.md)
* [Output](docs/output.md)
* [Development](docs/development.md)
* [Bug reports](https://github.com/returntocorp/semgrep/issues)

## Contribution

`semgrep` is LGPL-licensed, feel free to help out: [CONTRIBUTING](https://github.com/returntocorp/semgrep/blob/develop/CONTRIBUTING.md).

`semgrep` is a frontend to a larger program analysis library named [`pfff`](https://github.com/returntocorp/pfff/). `pfff` began and was open-sourced at [Facebook](https://github.com/facebookarchive/pfff) but is now archived. The primary maintainer now works at [r2c](https://r2c.dev). `semgrep` was originally named `sgrep` and was renamed to avoid collisons with existing projects.

## Commercial Support

`semgrep` is proudly supported by [r2c](https://r2c.dev). We're hiring!

Interested in a fully-supported, hosted version of semgrep? [Drop your email](https://forms.gle/dpUUvSo1WtELL8DW6) and we'll ping you!
