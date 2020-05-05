# semgrep

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
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| ✅ | ✅ | ✅ | ✅ | ✅ | Coming... | Coming... |

Example patterns:

| **Pattern** | **Matches** |
| :--- | :--- |
| `$X == $X` | `if (node.id == node.id): ...` |
| `requests.get(..., verify=False, ...)` | `requests.get(url, timeout=3, verify=False)` |
| `os.system(...)` | `from os import system; system('echo semgrep')` |
| `$ELEMENT.innerHTML` | ``el.innerHTML = "<img src='x' onerror='alert(`XSS`)'>";`` |
| `$TOKEN.SignedString([]byte("..."))` | `ss, err := token.SignedString([]byte("HARDCODED KEY"))` |

→ [see more example patterns in the semgrep-rules repository](https://github.com/returntocorp/semgrep-rules)

## Installation

Install `semgrep` with [Docker](https://docs.docker.com/install/):

```bash
docker pull returntocorp/semgrep
```

On OSX, binaries are available via [Homebrew](https://brew.sh/):

```bash
brew install returntocorp/semgrep/semgrep
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
$ docker run --rm -v "${PWD}:/home/repo" returntocorp/semgrep --lang python --pattern '$X == $X' test.py
test.py
rule:python.deadcode.eqeq-is-bad: useless comparison operation `node.id == node.id` or `node.id != node.id`.
3:        if node.id == node.id:  # Oops, supposed to be 'node_id'
```

### r2c-developed Rules

You can use rules developed by [r2c](https://r2c.dev) to search for issues in your codebase:

```bash
cd /path/to/code
docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep --config r2c
```

### Custom Rules

You can also [create your own rules](docs/configuration-files.md):

```bash
cd /path/to/code
docker run --rm -v "${PWD}:/home/repo" returntocorp/semgrep --generate-config
docker run --rm -v "${PWD}:/home/repo" returntocorp/semgrep
```

## Configuration

For simple patterns use the `--lang` and `--pattern` flags. This mode of
operation is useful for quickly iterating on a pattern on a single file or
folder:

```bash
docker run --rm -v "${PWD}:/home/repo" returntocorp/semgrep --lang javascript --pattern 'eval(...)' path/to/file.js
```

To fine-tune your searching, specify the `--help` flag:

```bash
docker run --rm returntocorp/semgrep --help
```

### Configuration Files

For advanced configuration use the `--config` flag. This flag automagically
handles a multitude of input configuration types:

* `--config <file|folder|yaml_url|tarball_url|registy_name>`

In the absence of this flag, a default configuration is loaded from `.sgrep.yml`
or multiple files matching `.sgrep/**/*.yml`.

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
are semantically equivalent

```python
subprocess.Popen(...)
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
docker run --rm -v "${PWD}:/home/repo" returntocorp/semgrep --config r2c
```

## Resources

* [`semgrep` presentation at HellaSecure](https://www.youtube.com/watch?v=M586wePrwYs) and [slides](https://bit.ly/hella-secure-semgrep) 
* [Pattern features documentation](docs/pattern-features.md)
* [Configuration files documentation](docs/configuration-files.md)
* [Integrations](docs/integrations.md)
* [Development](docs/development.md)
* [Bug reports](https://github.com/returntocorp/semgrep/issues)

## Contribution

`semgrep` is LGPL-licensed, feel free to help out: [CONTRIBUTING](https://github.com/returntocorp/sgrep/blob/develop/CONTRIBUTING.md).

`semgrep` is a frontend to a larger program analysis library named [pfff](https://github.com/returntocorp/pfff/), where it was named `sgrep`. pfff began and was open-sourced at [Facebook](https://github.com/facebookarchive/pfff) but is now archived and the primary maintainer works at [r2c](https://r2c.dev).

## Commercial Support

`semgrep` is proudly supported by [r2c](https://r2c.dev). We're hiring! 

Interested in a fully-supported, hosted version of semgrep? [Drop your email](https://forms.gle/dpUUvSo1WtELL8DW6) and we'll ping you!
