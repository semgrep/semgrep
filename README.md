<p align="center">
    <a href="https://semgrep.dev"><img src="semgrep.svg" height="100" alt="Semgrep logo"/></a>
</p>
<h3 align="center">
  Lightweight static analysis for many languages.
  </br>
  Find bugs and enforce code standards.
</h3>
<p align="center">
  <a href="https://formulae.brew.sh/formula/semgrep">
    <img src="https://img.shields.io/homebrew/v/semgrep?style=flat-square" alt="Homebrew" />
  </a>
  <a href="https://pypi.org/project/semgrep/">
    <img alt="PyPI" src="https://img.shields.io/pypi/v/semgrep?style=flat-square&color=blue">
  </a>
  <a href="https://r2c.dev/slack">
    <img src="https://img.shields.io/badge/slack-join-green?style=flat-square" alt="Issues welcome!" />
  </a>
  <a href="https://github.com/returntocorp/semgrep/issues/new/choose">
    <img src="https://img.shields.io/badge/issues-welcome-green?style=flat-square" alt="Issues welcome!" />
  </a>
  <a href="https://github.com/returntocorp/semgrep#readme">
    <img src="https://img.shields.io/github/stars/returntocorp/semgrep?label=GitHub%20Stars&style=flat-square" alt="1500+ GitHub stars" />
  </a>
  <a href="https://twitter.com/intent/follow?screen_name=r2cdev">
    <img src="https://img.shields.io/twitter/follow/r2cdev?label=Follow%20r2cdev&style=social&color=blue" alt="Follow @r2cdev" />
  </a>
</p>
</br>

Semgrep is a static analysis tool that excels at expressing code standards — without complicated queries — and surfacing bugs early in the development flow. Rules look like the code you’re searching without needing to understand abstract syntax trees or wrestle with regexes.

The [Semgrep Registry](https://semgrep.dev/explore) has 900+ rules written by the Semgrep community covering security, correctness, and performance bugs. No need to DIY unless you want to.

Semgrep runs offline, on uncompiled code.

## Language support

<p align="center">
Go · Java · JavaScript · JSON · Python</br>
Ruby (beta) · TypeScript (beta) · JSX (beta) · TSX (beta)
</br>
</br>
Visit <a href="TODO">Supported languages</a> for the complete list.

</p>


## Getting started

Visit [Getting started](https://semgrep.dev/docs/getting-started/) to quick start.

## Examples

Visit [Rule examples](https://semgrep.dev/docs/writing-rules/rule-ideas/) for use cases and ideas.

## Contributing

Visit [Contributing](https://semgrep.dev/docs/contributing/).

## Commercial use

Semgrep is LGPL 2.1 licensed and supported by [r2c](https://r2c.dev). Visit [FAQ](https://semgrep.dev/docs/faq/) to learn more.

## Usage

Visit [CLI API](TODO) for usage and exit code documentation.

## Upgrading

To upgrade, run the command below associated with how you installed Semgrep:

```sh
# Using Homebrew
$ brew upgrade semgrep

# Using pip
$ python3 -m pip install --upgrade semgrep

# Using Docker
$ docker pull returntocorp/semgrep:latest
```
