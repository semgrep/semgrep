# sgrep [![ci](https://github.com/returntocorp/sgrep/workflows/ci/badge.svg)](https://github.com/returntocorp/sgrep/actions?query=workflow%3Aci+branch%3Adevelop)

[![r2c community slack](https://img.shields.io/badge/r2c_slack-join-brightgreen?style=for-the-badge&logo=slack&labelColor=4A154B)](https://join.slack.com/t/r2c-community/shared_invite/enQtNjU0NDYzMjAwODY4LWE3NTg1MGNhYTAwMzk5ZGRhMjQ2MzVhNGJiZjI1ZWQ0NjQ2YWI4ZGY3OGViMGJjNzA4ODQ3MjEzOWExNjZlNTA)

[sgrep.live](https://sgrep.live/) - Try it now

[`sgrep`](https://sgrep.live/), for syntactical (and occasionnally semantic) grep, is a tool to help find bugs by specifying code patterns using a familiar
syntax. The idea is to mix the convenience of grep with the correctness and precision of a compiler frontend.

## Quick Examples

<table>
  <tr><td><b>pattern</b></td><td><b>will match code like</b></td></tr>
  <tr><td><code>$X == $X</code></td><td><code>if (node.id == node.id): ...</code></td></tr>
  <tr><td><code>foo(kwd1=1, kwd2=2, ...)</code></td><td><code>foo(kwd2=2, kwd1=1, kwd3=3)</code></td></tr>
  <tr><td><code>subprocess.open(...)</code></td><td><code>import subprocess as s; s.open(['foo'])</code></td></tr>
  <tr><td colspan=2><a href="https://github.com/returntocorp/sgrep-rules">see more examples in the sgrep-rules registry</a></td></tr>
</table>

## Supported Languages by Sgrep Feature

<table style="text-align:center">
<tr>
<td>

</td>
<td>
<b>go</b>
</td>
<td>
<b>python</b>
</td>
<td>
<b>js</b>
</td>
<td>
<b>java</b>
</td>
<td>
<b>php</b>
</td>
<td>
<b>ml</b>
</td>
<td>
<b>cpp</b>
</td>
<td>
<b>c</b>
</td>
</tr>
<tr>
<td>
'...' operator
</td>
<td>
✅
</td>
<td>
✅
</td>
<td>
✅
</td>
<td>
✅
</td>
<td>
🚧
</td>
<td>
🚧
</td>
<td>
🚧
</td>
<td>
🔶
</td>
</tr>
<tr>
<td>
Equivalences
</td>
<td>
🔶
</td>
<td>
🔶
</td>
<td>
🔶
</td>
<td>
🔶
</td>
<td>
🚧
</td>
<td>
🚧
</td>
<td>
🚧
</td>
<td>
🔶
</td>
</tr>
<tr>
<td>
Metavariables
</td>
<td>
✅
</td>
<td>
✅
</td>
<td>
✅
</td>
<td>
✅
</td>
<td>
🚧
</td>
<td>
🚧
</td>
<td>
🚧
</td>
<td>
✅
</td>
</tr>
<tr>
<td>
Others
</td>
<td>
🔶
</td>
<td>
✅
</td>
<td>
🔶
</td>
<td>
🔶
</td>
<td>
🚧
</td>
<td>
🚧
</td>
<td>
🚧
</td>
<td>
🚧
</td>
</tr>
</table>

- ✅ — Supported
- 🔶 — Partial support
- 🚧 — Under development

## Meetups

Want to learn more about sgrep? Check out these [slides from the r2c February meetup](https://r2c.dev/sgrep-public2.pdf)

## Installation

Too lazy to install? Try out [sgrep.live](https://sgrep.live)

### Docker

`sgrep` is packaged within a [docker container](https://hub.docker.com/r/returntocorp/sgrep), making installation as easy as [installing docker](https://docs.docker.com/install/).

## Quickstart

```bash
docker pull returntocorp/sgrep

cd /path/to/repo
# generate a template config file
docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep --generate-config

# look for findings
docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep

```

## Usage

### Rule Development

To rapidly iterate on a single pattern, you can test on a single file or folder. For example,

```bash
docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep -l python -e '$X == $X' path/to/file.py
```

Here, `sgrep` will search the target with the pattern `$X == $X` (which is a stupid equals check) and print the results to `stdout`. This also works for directories and will skip the file if parsing fails. You can specifiy the language of the pattern with `--lang javascript` for example.

To see more options

```bash
docker run --rm -v "${PWD}:/home/repo" returntocorp/sgrep --help
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
2. **Metavariables (\$X)**: abstract away code
3. **'...' operator:** abstract away sequences
4. **Knows about code equivalences:** one pattern can match many equivalent variations on the code
5. **Less is more:** abstract away additional details

## Patterns

Patterns are snippets of code with variables and other operators that will be parsed into an AST for that langauge and will be used to search for that pattern in code. See [patterns.md](docs/patterns.md) for full documentation.

### Metavariables

`$X`, `$FOO`, `$RETURNCODE` are all examples of metavariables. You can referance them later in your pattern and `sgrep` will ensure they match. **Metavariables can only contain uppercase ASCII characters**; `$x` and `$SOME_VALUE` are not valid metavariables.

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

Reports are welcome! Please open an issue on this project.

## Contributions

`sgrep` is LGPL-licensed and we would love your [contributions](docs/CONTRIBUTING.md). See [docs/development.md](docs/development.md)
