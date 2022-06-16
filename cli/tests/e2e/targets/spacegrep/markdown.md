<p align="center">
    <a href="https://semgrep.dev"><img src="semgrep.svg" height="100" alt="Semgrep logo"/></a>
</p>
<h3 align="center">
  Lightweight static analysis for many languages.
  </br>
  Find and block bug variants with rules that look like source code.
</h3>

<p align="center">
  <a href="#getting-started">Getting Started</a>
  <span> · </span>
  <a href="#Examples">Examples</a>
  <span> · </span>
  <a href="#resources">Resources</a>
  <br/>
  <a href="#usage">Usage</a>
  <span> · </span>
  <a href="#contributing">Contributing</a>
  <span> · </span>
  <a href="#commercial-support">Commercial Support</a>
</p>

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

<a href="https://semgrep.dev">Semgrep</a> tl;dr:

- A simple, customizable, and fast static analysis tool for finding bugs
- Combines the speed and customization of `grep` with the precision of traditional static analysis tools
- No painful domain-specific language; Semgrep rules look like the source code you’re targeting
- Batteries included with hundreds of existing community rules for OWASP Top 10 issues and common mistakes
- Runs in CI, at pre-commit, or in the editor
- Runs offline on uncompiled code

Semgrep supports:

| Go  | Java | JavaScript | JSON | Python | Ruby (beta) | JSX (beta) | C (alpha) | OCaml (alpha) |
| --- | ---- | ---------- | ---- | ------ | ----------- | ---------- | --------- | ------------- |


Semgrep is proudly supported by r2c. Learn more about a hosted version of Semgrep with an enterprise feature set at [r2c.dev](https://r2c.dev/).

## Getting Started

The best place to start with Semgrep and rule writing is its [Quick Start](https://semgrep.dev/editor). For a more in-depth introduction to its syntax and use cases visit the [Semgrep Tutorial](https://semgrep.dev/learn).

Semgrep can be installed using `brew`, `pip`, or `docker`:

```sh
# For macOS
$ brew install semgrep

# On Ubuntu/WSL/linux, we recommend installing via `pip`
$ python3 -m pip install semgrep

# To try Semgrep without installation run via Docker
$ docker run --rm -v "${PWD}:/src" returntocorp/semgrep --help
```

To confirm installation and get an overview of Semgrep's functionality run with `--help`:

```
$ semgrep --help
```

Once installed, Semgrep can be run with single rule patterns or entire rule sets:

```sh
# Check for Python == where the left and right hand sides are the same (often a bug)
$ semgrep -e '$X == $X' --lang=py path/to/src

# Run a ruleset with rules for many languages
$ semgrep --config=https://semgrep.dev/p/r2c-CI path/to/src
```

Explore the Semgrep Registry of rules and CI integrations at [semgrep.dev](https://semgrep.dev/packs).

## Examples

| Use case                          | Semgrep rule                                                                                                                                                                                                                                                                                                                                           |
| :-------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Ban dangerous APIs                | [Prevent use of exec](https://semgrep.live/clintgibler:no-exec)                                                                                                                                                                                                                                                                                        |
| Search routes and authentiation   | [Extract Spring routes](https://semgrep.live/clintgibler:spring-routes)                                                                                                                                                                                                                                                                                |
| Enforce the use secure defaults   | [Securely set Flask cookies](https://semgrep.dev/dlukeomalley:flask-set-cookie)                                                                                                                                                                                                                                                                        |
| Enforce project best-practices    | [Use assertEqual for == checks](https://semgrep.dev/dlukeomalley:use-assertEqual-for-equality), [Always check subprocess calls](https://semgrep.dev/dlukeomalley:unchecked-subprocess-call)                                                                                                                                                            |
| Codify project-specific knowledge | [Verify transactions before making them](https://semgrep.dev/dlukeomalley:verify-before-make)                                                                                                                                                                                                                                                          |
| Audit security hotspots           | [Finding XSS in Apache Airflow](https://semgrep.live/ievans:airflow-xss), [Hardcoded credentials](https://semgrep.dev/dlukeomalley:hardcoded-credentials)                                                                                                                                                                                              |
| Audit configuration files         | [Find S3 ARN uses](https://semgrep.dev/dlukeomalley:s3-arn-use)                                                                                                                                                                                                                                                                                        |
| Migrate from deprecated APIs      | [DES is deprecated](https://semgrep.dev/editor?registry=java.lang.security.audit.crypto.des-is-deprecated), [Deprecated Flask APIs](https://semgrep.dev/editor?registry=python.flask.maintainability.deprecated.deprecated-apis), [Deprecated Bokeh APIs](https://semgrep.dev/editor?registry=python.bokeh.maintainability.deprecated.deprecated_apis) |
| Apply automatic fixes             | [Use listenAndServeTLS](https://semgrep.live/clintgibler:use-listenAndServeTLS)                                                                                                                                                                                                                                                                        |

### Try it out

Give some rulesets a spin by running on known vulnerable repositories:

```bash
# juice-shop, a vulnerable Node.js + Express app
$ git clone https://github.com/bkimminich/juice-shop
$ semgrep -f https://semgrep.dev/p/r2c-security-audit juice-shop
```

```bash
# railsgoat, a vulnerable Ruby on Rails app
$ git clone https://github.com/OWASP/railsgoat
$ semgrep -f https://semgrep.dev/p/r2c-security-audit railsgoat
```

```bash
# govwa, a vulnerable Go app
$ git clone https://github.com/0c34/govwa
$ semgrep -f https://semgrep.dev/p/r2c-security-audit govwa
```

```bash
# vulnerable Python+Flask app
$ git clone https://github.com/we45/Vulnerable-Flask-App
$ semgrep -f https://semgrep.dev/p/r2c-security-audit Vulnerable-Flask-App
```

```bash
# WebGoat, a vulnerable Java+Sprint app
$ git clone https://github.com/WebGoat/WebGoat
$ semgrep -f https://semgrep.dev/p/r2c-security-audit WebGoat
```

## Resources

Learn more:

- [Live Editor](https://semgrep.dev/editor)
- [Semgrep Registry](https://semgrep.dev/r)
- [Documentation](docs/README.md)
- [r2c YouTube channel](https://www.youtube.com/channel/UC5ahcFBorwzUTqPipFhjkWg)

Get in touch:

- Submit a [bug report](https://github.com/returntocorp/semgrep/issues)
- Join the [Semgrep Slack](https://r2c.dev/slack) to say "hi" or ask questions

## Usage

### Command Line Options

See `semgrep --help` for command line options.

### Exit Codes

`semgrep` may exit with the following exit codes:

- `0`: Semgrep ran successfully and found no errors
- `1`: Semgrep ran successfully and found issues in your code
- \>=`2`: Semgrep failed to run

### Upgrading

To upgrade, run the command below associated with how you installed Semgrep:

```sh
# Using Homebrew
$ brew upgrade semgrep

# Using pip
$ python3 -m pip install --upgrade semgrep

# Using Docker
$ docker pull returntocorp/semgrep:latest
```

## Contributing

Semgrep is LGPL-licensed and we welcome contributions.

To start contributing, first please make sure you read and agree with the [Contributor Covenant Code of Conduct](https://github.com/returntocorp/semgrep/blob/develop/CODE_OF_CONDUCT.md).
Then check out a few ways you can get involved:

- [File an issue](https://github.com/returntocorp/semgrep/issues/new/choose)
- Fix a bug — pick from the [good first issues](https://github.com/returntocorp/semgrep/issues?q=is%3Aopen+is%3Aissue+label%3A%22good+first+issue%22) or work on any of the [currently open bugs](https://github.com/returntocorp/semgrep/issues?q=is%3Aopen+is%3Aissue+label%3Abug)
- Add a feature — see the [enhancement issues](https://github.com/returntocorp/semgrep/issues?q=is%3Aopen+is%3Aissue+label%3Aenhancement) for inspiration
- Update the [docs](https://github.com/returntocorp/semgrep/tree/develop/docs)
- Help each other in the [community Slack](https://r2c.dev/slack)

Please see the [contribution guidelines](https://github.com/returntocorp/semgrep/blob/develop/doc/README.md) for info about the development workflow, testing, and making PRs.

## Commercial Support

Semgrep is a frontend to a larger program analysis library named [`pfff`](https://github.com/returntocorp/pfff/). `pfff` began and was open-sourced at [Facebook](https://github.com/facebookarchive/pfff) but is now archived. The primary maintainer now works at [r2c](https://r2c.dev). Semgrep was originally named `sgrep` and was renamed to avoid collisons with existing projects.

Semgrep is supported by [r2c](https://r2c.dev). We're hiring!

Interested in a fully-supported, hosted version of Semgrep? [Drop your email](https://forms.gle/dpUUvSo1WtELL8DW6) and we'll be in touch!
