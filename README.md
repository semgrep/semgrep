</br>
<p align="center">
    <a href="https://semgrep.dev"><img src="https://raw.githubusercontent.com/returntocorp/semgrep/develop/semgrep.svg" height="100" alt="Semgrep logo"/></a>
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
  <a href="https://semgrep.dev/docs/">
      <img src="https://img.shields.io/badge/docs-semgrep.dev-purple?style=flat-square" alt="Documentation" />
  </a>
  <a href="https://r2c.dev/slack">
    <img src="https://img.shields.io/badge/slack-1.2k%20members-green?style=flat-square" alt="Join Semgrep community Slack" />
  </a>
  <a href="https://github.com/returntocorp/semgrep/issues/new/choose">
    <img src="https://img.shields.io/badge/issues-welcome-green?style=flat-square" alt="Issues welcome!" />
  </a>
  <a href="https://github.com/returntocorp/semgrep#readme">
    <img src="https://img.shields.io/github/stars/returntocorp/semgrep?label=GitHub%20Stars&style=flat-square" alt="Star Semgrep on GitHub" />
  </a>
  <a href="https://hub.docker.com/r/returntocorp/semgrep">
    <img src="https://img.shields.io/docker/pulls/returntocorp/semgrep.svg?style=flat-square" alt="Docker Pulls" />
  </a>
  <a href="https://twitter.com/intent/follow?screen_name=r2cdev">
    <img src="https://img.shields.io/twitter/follow/r2cdev?label=Follow%20r2cdev&style=social&color=blue" alt="Follow @r2cdev on Twitter" />
  </a>
</p>
</br>

Semgrep is a fast, open-source, static analysis tool for finding bugs and enforcing code standards at editor, commit, and CI time. [Get started →.](#getting-started)

Semgrep analyzes code locally on your computer or in your build environment: **code is never uploaded**.

Its rules look like the code you already write; no abstract syntax trees, regex wrestling, or painful DSLs. Here's a quick rule for finding Python `print()` statements, run it online in Semgrep's Playground by clicking the image:

<p align="center">
    <a href="https://semgrep.dev/s/ievans:print-to-logger"><img src="https://github.com/returntocorp/semgrep/blob/develop/doc/playground-example.png" width="582" alt="Semgrep rule example for finding Python print() statements"/></a>
</p>

The Semgrep ecosystem includes:

- Semgrep - the open-source command line tool at the heart of everything (this project)
- [Semgrep CI](https://semgrep.dev/docs/semgrep-ci/) - a specialized Docker image for running Semgrep in CI environments
- [Semgrep Playground](https://semgrep.dev/editor) - an online interactive rule builder for writing and sharing rules
- [Semgrep Registry](https://semgrep.dev/explore) - 2,000+ community-driven rules covering security, correctness, and performance bugs
- [Semgrep App](https://semgrep.dev/manage) - deploy, manage, and monitor Semgrep at scale with free and paid tiers.

Join 100,000 other developers and security engineers already using Semgrep at companies like Chef, Dropbox, Figma, HashiCorp, Snowflake, and Trail of Bits. Also check out [tools powered by Semgrep](https://semgrep.dev/docs/extensions/#semgrep-as-an-engine)!

Semgrep is developed and commercially supported by [r2c, a software security company](https://r2c.dev).

### Language support

<h4 align="center">General availability</h4>
<p align="center">
C# · Go · Java · JavaScript · JSX · JSON · PHP · Python · Ruby · Scala · TypeScript · TSX</br>
</p>
<h4 align="center">Beta & experimental</h4>
<p align="center">
See <a href="https://semgrep.dev/docs/supported-languages/">supported languages</a> for the complete list.
</p>

### Getting started

To install Semgrep use Homebrew or pip, or run without installation via Docker:

```sh
# For macOS
$ brew install semgrep

# For Ubuntu/WSL/Linux/macOS
$ python3 -m pip install semgrep

# To try Semgrep without installation run via Docker
$ docker run --rm -v "${PWD}:/src" returntocorp/semgrep
```

Once installed, Semgrep can run with single rules or entire rulesets. Visit [Docs > Running rules](https://semgrep.dev/docs/running-rules/) to learn more or try the following:

```sh
# Check for Python == where the left and right hand sides are the same (often a bug)
$ semgrep -e '$X == $X' --lang=py path/to/src

# Fetch rules automatically by setting the `--config auto` flag.
# This will fetch rules relevant to your project from Semgrep Registry.
# The name of your project will be sent to Semgrep Registry as an identifier
# to make selecting relevant rules fast next time;
# source code will not be uploaded.
$ semgrep --config auto
```

Visit the [full documentation](https://semgrep.dev/docs/getting-started/) to learn more.

### Rule examples

Visit [Docs > Rule examples](https://semgrep.dev/docs/writing-rules/rule-ideas/) for use cases and ideas.

| Use case                          | Semgrep rule                                                                                                                                                                                                                                                                                                                                           |
| :-------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Ban dangerous APIs                | [Prevent use of exec](https://semgrep.dev/s/clintgibler:no-exec)                                                                                                                                                                                                                                                                                       |
| Search routes and authentication  | [Extract Spring routes](https://semgrep.dev/s/clintgibler:spring-routes)                                                                                                                                                                                                                                                                               |
| Enforce the use secure defaults   | [Securely set Flask cookies](https://semgrep.dev/s/dlukeomalley:flask-set-cookie)                                                                                                                                                                                                                                                                      |
| Tainted data flowing into sinks   | [ExpressJS dataflow into sandbox.run](https://semgrep.dev/s/ievans:simple-taint-dataflow)                                                                                                                                                                                                                                                              |
| Enforce project best-practices    | [Use assertEqual for == checks](https://semgrep.dev/s/dlukeomalley:use-assertEqual-for-equality), [Always check subprocess calls](https://semgrep.dev/s/dlukeomalley:unchecked-subprocess-call)                                                                                                                                                        |
| Codify project-specific knowledge | [Verify transactions before making them](https://semgrep.dev/s/dlukeomalley:verify-before-make)                                                                                                                                                                                                                                                        |
| Audit security hotspots           | [Finding XSS in Apache Airflow](https://semgrep.dev/s/ievans:airflow-xss), [Hardcoded credentials](https://semgrep.dev/s/dlukeomalley:hardcoded-credentials)                                                                                                                                                                                           |
| Audit configuration files         | [Find S3 ARN uses](https://semgrep.dev/s/dlukeomalley:s3-arn-use)                                                                                                                                                                                                                                                                                      |
| Migrate from deprecated APIs      | [DES is deprecated](https://semgrep.dev/editor?registry=java.lang.security.audit.crypto.des-is-deprecated), [Deprecated Flask APIs](https://semgrep.dev/editor?registry=python.flask.maintainability.deprecated.deprecated-apis), [Deprecated Bokeh APIs](https://semgrep.dev/editor?registry=python.bokeh.maintainability.deprecated.deprecated_apis) |
| Apply automatic fixes             | [Use listenAndServeTLS](https://semgrep.dev/s/clintgibler:use-listenAndServeTLS)                                                                                                                                                                                                                                                                       |

### Extensions

Visit [Docs > Extensions](https://semgrep.dev/docs/extensions/) to learn about Semgrep in your editor or pre-commit. When integrated into CI and configured to scan pull requests, Semgrep will only report issues introduced by that pull request; this lets you start using Semgrep without fixing or ignoring pre-existing issues!

### Documentation

Browse the full Semgrep [documentation on the website](https://semgrep.dev/docs). If you’re new to Semgrep, check out [Docs > Getting started](https://semgrep.dev/docs/getting-started/) or the [interactive tutorial](https://semgrep.dev/learn).

### Metrics

Using remote configuration from the [Registry](https://semgrep.dev/r) (like `--config=p/ci`) reports pseudonymous rule metrics to semgrep.dev.

Using configs from local files (like `--config=xyz.yml`) does **not** enable metrics.

To disable Registry rule metrics, use `--metrics=off`.

[PRIVACY.md](PRIVACY.md) describes the principles that guide data-collection decisions and the breakdown of the data that are and are not collected when the metrics are enabled.

### More

- [Frequently asked questions (FAQs)](https://semgrep.dev/docs/faq/)
- [Contributing](https://semgrep.dev/docs/contributing/how-to-contribute/)
- [Build instructions for developers](INSTALL.md)
- [Ask questions in the r2c Community Slack](https://r2c.dev/slack)
- [CLI reference and exit codes](https://semgrep.dev/docs/cli-usage)
- [r2c YouTube channel with Semgrep presentation videos](https://www.youtube.com/channel/UC5ahcFBorwzUTqPipFhjkWg)
- [License (LGPL-2.1)](LICENSE)

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
