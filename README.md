# sgrep

`sgrep` is a powerful tool to search code for *s*ymantic patterns. `sgrep` is extensible, polyglot, and rapidly evolving to meet the needs of product security teams and developers. `sgrep` is under active support and we invite you to reach out to [sgrep@r2c.dev](mailto:sgrep@r2c.dev) for support.

## Installation

### Docker

`sgrep` is packaged within a docker container, making installation as easy as [installing docker](https://docs.docker.com/install/).

### Mac

```bash
brew install sgrep # coming soon
```

## Quickstart

```bash
cd /path/to/repo
vim .sgrep.yml

docker run --rm -v $(pwd):/home/repo returntocorp/sgrep

```

## Usage

### Rule Development

To rapidly iterate on a single pattern, you can test on a single file or folder. For example,

```bash
docker run --rm -v $(pwd):/home/repo returntocorp/sgrep -e '$X == $X' path/to/login.py
```

Here, `sgrep` will search `login.py` pattern `$X == $X` (which is a stupid equals check) and print the results to `stdout`. This also works for directories and will skip the file if parsing fails. You can specifiy the language of the pattern with `--lang javascript` for example.

To see more options

```bash
docker run --rm returntocorp/sgrep --help
```

### Config Files

#### Format

TODO

#### sgrep Registry

r2c provides a registry of config files tuned using our Massive Analysis Platform on thousands of repositories. To use:

```bash

sgrep --config r2c

```

### Default

Default configs are loaded from `.sgrep.yml` or multiple files matching `.sgrep/*.yml` and can be overridden by using `--config <file|folder|url|registy_name>`

## Patterns

Patterns are snippets of code with variables and other operators that will be parsed into an AST for that langauge and will be used to search for that pattern in code.

### Metavariables

TODO

#### Operators

TODO

#### Equivalences

`sgrep` automatically searches for code that is semantically equivalent. For example, a pattern for

```python
$F = open($X, ...)
$F.read()
```

will match

```python
with open('foo.txt', 'rb') as f:
    f.read()
```

and other semantically equivalent configurations.

## Outstanding Issues

- TODO
