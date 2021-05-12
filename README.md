<p align="center">
    <a href="https://semgrep.dev"><img src="https://raw.githubusercontent.com/returntocorp/semgrep/develop/semgrep.svg" height="150" alt="Semgrep logo"/></a>
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
  <a href="https://hub.docker.com/r/returntocorp/semgrep">
  <img src="https://img.shields.io/docker/pulls/returntocorp/semgrep.svg" />
    </a>
  <a href="https://twitter.com/intent/follow?screen_name=r2cdev">
    <img src="https://img.shields.io/twitter/follow/r2cdev?label=Follow%20r2cdev&style=social&color=blue" alt="Follow @r2cdev" />
  </a>
</p>
</br>

Semgrep is a fast, open-source, static analysis tool that finds bugs and enforces code standards at editor, commit, and CI time. Precise rules look like the code you’re searching; no more traversing abstract syntax trees, wrestling with regexes, or using a painful DSL. Code analysis is performed locally (code is not uploaded) and Semgrep runs on uncompiled code.

The [Semgrep Registry](https://semgrep.dev/explore) has 1,000+ rules written by the Semgrep community covering security, correctness, and performance bugs. No need to DIY unless you want to.

Semgrep is used in production everywhere from one-person startups to multi-billion dollar companies; it’s the engine inside tools like [NodeJsScan](https://semgrep.dev/p/nodejsscan). See tools [powered by Semgrep](https://semgrep.dev/docs/integrations/#semgrep-as-an-engine).

Semgrep is developed and commercially supported by [r2c, a software security company](https://r2c.dev). r2c’s hosted service, [Semgrep App](https://semgrep.dev), lets organizations easily deploy in CI, manage rules across many projects, monitor the efficacy of code policy, and integrate with 3rd-party services. r2c offers free and paid hosted tiers ([see pricing](https://r2c.dev/pricing)).

### Language support


<h4 align="center">General availability</h4>
<p align="center">
Go · Java · JavaScript · JSX · JSON · Python · Ruby · TypeScript · TSX</br>
</p>
<h4 align="center">Beta & experimental</h4>
<p align="center">
See <a href="https://semgrep.dev/docs/status/">supported languages</a> for the complete list.
</p>

### Getting started

To install Semgrep use Homebrew or pip, or run without installation via Docker:

```sh
# For macOS
$ brew install semgrep

# For Ubuntu/WSL/Linux/macOS
$ python3 -m pip install semgrep

# To try Semgrep without installation run via Docker
$ docker run --rm -v "${PWD}:/src" returntocorp/semgrep --help
```

Once installed, Semgrep can run with single rules or entire rulesets. Visit [Running rules](https://semgrep.dev/docs/running-rules/) to learn more or try the following:

```sh
# Check for Python == where the left and right hand sides are the same (often a bug)
$ semgrep -e '$X == $X' --lang=py path/to/src

# Run the r2c-ci ruleset (with rules for many languages) on your own code!
$ semgrep --config=p/r2c-ci path/to/src
```

Visit [Getting started](https://semgrep.dev/docs/getting-started/) to learn more.

### Rule examples

Visit [Rule examples](https://semgrep.dev/docs/writing-rules/rule-ideas/) for use cases and ideas. There is also an excellent [interactive tutorial](https://semgrep.dev/learn).

| Use case                          | Semgrep rule                                                                                                                                                                                                                                                                                                                                           |
| :-------------------------------- | :----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Ban dangerous APIs                | [Prevent use of exec](https://semgrep.dev/s/clintgibler:no-exec)                                                                                                                                                                                                                                                                                         |
| Search routes and authentication   | [Extract Spring routes](https://semgrep.dev/s/clintgibler:spring-routes)                                                                                                                                                                                                                                                                                 |
| Enforce the use secure defaults   | [Securely set Flask cookies](https://semgrep.dev/s/dlukeomalley:flask-set-cookie)                                                                                                                                                                                                                                                                        |
| Enforce project best-practices    | [Use assertEqual for == checks](https://semgrep.dev/s/dlukeomalley:use-assertEqual-for-equality), [Always check subprocess calls](https://semgrep.dev/s/dlukeomalley:unchecked-subprocess-call)                                                                                                                                                            |
| Codify project-specific knowledge | [Verify transactions before making them](https://semgrep.dev/s/dlukeomalley:verify-before-make)                                                                                                                                                                                                                                                          |
| Audit security hotspots           | [Finding XSS in Apache Airflow](https://semgrep.dev/s/ievans:airflow-xss), [Hardcoded credentials](https://semgrep.dev/s/dlukeomalley:hardcoded-credentials)                                                                                                                                                                                               |
| Audit configuration files         | [Find S3 ARN uses](https://semgrep.dev/s/dlukeomalley:s3-arn-use)                                                                                                                                                                                                                                                                                        |
| Migrate from deprecated APIs      | [DES is deprecated](https://semgrep.dev/editor?registry=java.lang.security.audit.crypto.des-is-deprecated), [Deprecated Flask APIs](https://semgrep.dev/editor?registry=python.flask.maintainability.deprecated.deprecated-apis), [Deprecated Bokeh APIs](https://semgrep.dev/editor?registry=python.bokeh.maintainability.deprecated.deprecated_apis) |
| Apply automatic fixes             | [Use listenAndServeTLS](https://semgrep.dev/s/clintgibler:use-listenAndServeTLS)   


### Integrations

Visit [Integrations](https://semgrep.dev/docs/integrations/) to learn about Semgrep editor, commit, and CI integrations. When integrated into CI and configured to scan pull requests, Semgrep will only report issues introduced by that pull request; this lets you start using Semgrep without fixing or ignoring pre-existing issues!


### Documentation

Browse the full Semgrep [documentation on the website](https://semgrep.dev/docs). If you’re new to Semgrep, check out [Getting started](https://semgrep.dev/docs/getting-started/) or the [interactive tutorial](https://semgrep.dev/learn).


### More

* [Frequently asked questions (FAQs)](https://semgrep.dev/docs/faq/)
* [Contributing](https://semgrep.dev/docs/contributing/)
* [Ask questions in the r2c Community Slack](https://r2c.dev/slack)
* [CLI reference and exit codes](https://semgrep.dev/docs/cli-usage)
* [r2c YouTube channel with Semgrep presentation videos](https://www.youtube.com/channel/UC5ahcFBorwzUTqPipFhjkWg)
* [License (LGPL-2.1)](LICENSE)


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
