# sgrep

[![CircleCI](https://circleci.com/gh/returntocorp/sgrep.svg?style=svg)](https://circleci.com/gh/returntocorp/sgrep)
[![r2c Community Slack](https://img.shields.io/badge/r2c_slack-join-brightgreen?style=flat&logo=slack&labelColor=4A154B)](https://join.slack.com/t/r2c-community/shared_invite/enQtNjU0NDYzMjAwODY4LWE3NTg1MGNhYTAwMzk5ZGRhMjQ2MzVhNGJiZjI1ZWQ0NjQ2YWI4ZGY3OGViMGJjNzA4ODQ3MjEzOWExNjZlNTA)

`sgrep` is a tool for easily detecting and preventing bugs and anti-patterns in
your codebase. It combines the convenience of `grep` with the correctness of
syntactical and semantic search. Quickly write rules so you can code with
confidence.

**Try it now:** [https://sgrep.live](https://sgrep.live/)

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
| `os.system(...)` | `from os import system; system('echo sgrep')` |
| `$ELEMENT.innerHTML` | ``el.innerHTML = "<img src='x' onerror='alert(`XSS`)'>";`` |
| `$TOKEN.SignedString([]byte("..."))` | `ss, err := token.SignedString([]byte("HARDCODED KEY"))` |

## Installation

Install `sgrep` with [Docker](https://docs.docker.com/install/):

```
$ docker pull returntocorp/sgrep
```

And double check that it was installed correctly:

```
$ docker run --rm returntocorp/sgrep --help
```
### Installation with Brew (Experimental)

```bash
brew tap returntocorp/sgrep
brew install semgrep
```

## Usage

Start with a simple example:

```
$ cat << EOF > test.py
a = 1
b = 2
if a == a:  # oops, supposed to be a == b
    print('sgrep test')
EOF
```

```
$ docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep --lang python --pattern '$X == $X' test.py
test.py
3:if a == a:  # oops, supposed to be a == b
```

From here you can use our rules to search for issues in your codebase:

```
$ cd /path/to/code
$ docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep --config r2c
```

You can also [create your own rules](docs/config/advanced.md):

```
$ cd /path/to/code
$ docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep --generate-config
$ docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep
```

## Configuration

For simple patterns use the `--lang` and `--pattern` flags. This mode of
operation is useful for quickly iterating on a pattern on a single file or
folder:

```
$ docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep --lang javascript --pattern 'eval(...)' path/to/file.js
```

To fine-tune your searching, specify the `--help` flag:

```
$ docker run --rm returntocorp/sgrep --help
```

### Configuration Files

For advanced configuration use the `--config` flag. This flag automagically
handles a multitude of input types:

* `--config <file|folder|yaml_url|tarball_url|registy_name>`

In the absense of this flag, a default configuration is loaded from `.sgrep.yml`
or multiple files matching `.sgrep/**/*.yml`.

#### Operators

Configuration files make use of two primary operators:

* **Metavariables like `$X`, `$WIDGET`, or `$USERS`.** Metavariable names can
only contain uppercase characters - names like `$x` or `$SOME_VALUE` are
invalid.  Metavariables are used to track a variable across a specific code
scope.
* **The `...` (ellipsis) operator.** The ellipsis operator abstracts away
sequences so you don't have to sweat the details of a particular code pattern.

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

**For a more complete introduction to the configuration format please see the
[advanced configuration documentation](docs/config/advanced.md).**

#### Equivalences

Equivalences are another key concept in `sgrep`. `sgrep` automatically searches
for code that is semantically equivalent. For example, the following patterns
are semantically equivalent

```python
subprocess.Popen(...)
```

```python
from subprocess import Popen as sub_popen
result = sub_popen("ls")
```

For a full list of `sgrep` feature support by language see the
[language matrix](docs/matrix.md).

### Registry

As mentioned above, you may also specify a registry name as configuration.
[r2c](https://r2c.dev) provides a [registry](https://github.com/returntocorp/sgrep-rules)
of configuration files. These rules have been tuned on thousands of repositories
using our [analysis platform](https://app.r2c.dev).

```
$ docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep --config r2c
```

## Resources

* [r2c `sgrep` meetup slides](https://web-assets.r2c.dev/sgrep/r2c-sgrep-meetup-feb-2020.pdf)
* [Simple configuration documentation](docs/config/simple.md)
* [Advanced configuration documentation](docs/config/advanced.md)
* [Integrations](docs/integrations.md)
* [Development](docs/development.md)
* [Bug reports](https://github.com/returntocorp/sgrep/issues)

## Contribution

`sgrep` is LGPL-licensed, feel free to help out: [CONTRIBUTING](https://github.com/returntocorp/sgrep/blob/develop/CONTRIBUTING.md).
