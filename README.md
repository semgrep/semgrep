# sgrep

[![r2c community slack](https://img.shields.io/badge/r2c_slack-join-brightgreen?style=for-the-badge&logo=slack&labelColor=4A154B)](https://join.slack.com/t/r2c-community/shared_invite/enQtNjU0NDYzMjAwODY4LWE3NTg1MGNhYTAwMzk5ZGRhMjQ2MzVhNGJiZjI1ZWQ0NjQ2YWI4ZGY3OGViMGJjNzA4ODQ3MjEzOWExNjZlNTA)

`sgrep`, for syntactical (and occasionnally semantic) grep, is a tool to help find bugs by specifying code patterns using a familiar
syntax. The idea is to mix the convenience of grep with the correctness and precision of a compiler frontend.

As an example, the pattern `$X == $X` can be used to find useless equality checks. The pattern `subprocess.open(...)` will fire on cases even where imported modules are renamed, like `import subprocess as s; s.open(['foo'])`.

## Supported Languages

<table>
  <tr>
   <td>Javascript</td>
   <td>Python</td>
   <td>Go</td>
   <td>Java</td>
   <td>C</td>
   <td>Ruby</td>
   <td>Scala</td>
</tr>
  <tr>
   <td>✅</td>
      <td>✅</td>
   <td>✅</td>
   <td>✅</td>
   <td>✅</td>
   <td>coming</td>
   <td>coming</td>
 </tr>
 </table>

## Meetups

Want to learn more about sgrep? Check out thes [slides from the r2c February meetup](https://r2c.dev/sgrep-public2.pdf)

## Installation

### Docker

`sgrep` is packaged within a [docker container](https://hub.docker.com/r/returntocorp/sgrep), making installation as easy as [installing docker](https://docs.docker.com/install/).

## Quickstart

```bash
docker pull returntocorp/sgrep

cd /path/to/repo
# generate a template config file
docker run --rm -v $(pwd):/home/repo returntocorp/sgrep --generate-config

# look for findings
docker run --rm -v $(pwd):/home/repo returntocorp/sgrep

```

## Usage

### Rule Development

To rapidly iterate on a single pattern, you can test on a single file or folder. For example,

```bash
docker run --rm -v $(pwd):/home/repo returntocorp/sgrep -e '$X == $X' path/to/file.py
```

Here, `sgrep` will search the target with the pattern `$X == $X` (which is a stupid equals check) and print the results to `stdout`. This also works for directories and will skip the file if parsing fails. You can specifiy the language of the pattern with `--lang javascript` for example.

To see more options

```bash
docker run --rm -v $(pwd):/home/repo returntocorp/sgrep --help
```

### Config Files

#### Format

See [config.md](docs/config.md) for example configuration files and details on the syntax.

#### sgrep Registry

[r2c](https://r2c.dev) provides a [registry](https://github.com/returntocorp/sgrep-rules) of config files tuned using our [analysis platform](https://app.r2c.dev) on thousands of repositories. To use:

```bash

sgrep --config r2c

```

### Default

Default configs are loaded from `.sgrep.yml` or multiple files matching `.sgrep/**/*.yml` and can be overridden by using `--config <file|folder|yaml_url|tarball_url|registy_name>`

## Design

Sgrep has a design philosophy that emphasizes simplicity and a single pattern being as expressive as possible:

1. **Use concrete code syntax:** easy to learn
2. **Metavariables ($X)**: abstract away code
3. **'...' operator:** abstract away sequences
4. **Knows about code equivalences:** one pattern can match many equivalent variations on the code
5. **Less is more:** abstract away additional details

## Patterns

Patterns are snippets of code with variables and other operators that will be parsed into an AST for that langauge and will be used to search for that pattern in code. See [patterns.md](docs/patterns.md) for full documentation.

### Metavariables

`$X`, `$FOO`, `$RETURN_CODE` are all examples of metavariables and you can referance them later in your pattern and `sgrep` will ensure they match

#### Operators

`...` is the primary "match anything" operator

#### Equivalences

`sgrep` automatically searches for code that is semantically equivalent. For example, a pattern for

```sgrep
subprocess.open(...)
```

will match

```python
from subprocess import open as
 sub_open
result = sub_open(“ls”)
```

and other semantically equivalent configurations.

## Integrations

See [integrations.md](docs/integrations.md)

## Bug Reports

Reports are welcome! Please open an github issue on this project.

## Contributions

`sgrep` is LGPL-licensed and we would love your [contributions](CONTRIBUTING.md). See [docs/development.md](docs/development.md)
