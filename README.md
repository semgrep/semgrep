<br />
<p align="center">
  <a href="https://semgrep.dev">
    <picture>
      <source media="(prefers-color-scheme: light)" srcset="images/semgrep-logo-light.svg">
      <source media="(prefers-color-scheme: dark)" srcset="images/semgrep-logo-dark.svg">
      <img src="https://raw.githubusercontent.com/semgrep/semgrep/develop/images/semgrep-logo-light.svg" height="100" alt="Semgrep logo"/>
    </picture>
  </a>
</p>
<h2 align="center">
  Code scanning at ludicrous speed.
</h2>
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
  <a href="https://go.semgrep.dev/slack">
    <img src="https://img.shields.io/badge/slack-3.5k%20members-green?style=flat-square" alt="Join Semgrep community Slack" />
  </a>
  <a href="https://github.com/semgrep/semgrep/issues/new/choose">
    <img src="https://img.shields.io/badge/issues-welcome-green?style=flat-square" alt="Issues welcome!" />
  </a>
  <a href="https://github.com/semgrep/semgrep#readme">
    <img src="https://img.shields.io/github/stars/semgrep/semgrep?label=GitHub%20Stars&style=flat-square" alt="Star Semgrep on GitHub" />
  </a>
  <a href="https://hub.docker.com/r/semgrep/semgrep">
    <img src="https://img.shields.io/docker/pulls/semgrep/semgrep.svg?style=flat-square" alt="Docker Pulls" />
  </a>
  <a href="https://hub.docker.com/r/returntocorp/semgrep">
    <img src="https://img.shields.io/docker/pulls/returntocorp/semgrep.svg?style=flat-square" alt="Docker Pulls (Old)" />
  </a>
  <a href="https://twitter.com/intent/follow?screen_name=semgrep">
    <img src="https://img.shields.io/twitter/follow/semgrep?label=Follow%20semgrep&style=social&color=blue" alt="Follow @semgrep on Twitter" />
  </a>
</p>
</br>

This repository contains the source code for Semgrep OSS (open-source software). Semgrep OSS is a fast, open-source, static analysis tool for searching code, finding bugs, and enforcing code standards at editor, commit, and CI time. Semgrep is a semantic grep for code: where `grep "2"` would only match the exact string _2_, Semgrep would [match `x = 1; y = x + 1` when searching for _2_](https://semgrep.dev/playground/s/5rKgj). And it does this in 30+ languages! Semgrep rules look like the code you already write; no abstract syntax trees, regex wrestling, or painful DSLs: read more below.

For companies who need SAST, SCA, and Secret scanning, we provide a product suite on top of Semgrep OSS that scans code and package dependencies for known issues, software vulnerabilities, and finds secrets with high accuracy:

- [**Semgrep Code**](https://semgrep.dev/products/semgrep-code/) to find bugs & vulnerabilities using the deeper, interfile-analysis enabled [Pro engine](https://semgrep.dev/products/pro-engine/) and high-accuracy Pro rules in addition to the community rules
- [**Semgrep Supply Chain**](https://semgrep.dev/products/semgrep-supply-chain/) to find dependencies with known vulnerabilities function-level reachability analysis
- [**Semgrep Secrets**](https://semgrep.dev/products/semgrep-secrets/) to find hard-coded credentials that shouldn't be checked into source code

Semgrep analyzes code locally on your computer or in your build environment: **by default, code is never uploaded**. [Get started →.](#getting-started-)

<a href="#option-1-getting-started-from-the-cli">
<img src="https://raw.githubusercontent.com/semgrep/semgrep/develop/images/semgrep-scan-cli.jpg" alt="Semgrep CLI image"/></a>

### Language support

**Semgrep Code** supports 30+ languages, including:

Apex · Bash · C · C++ · C# · Clojure · Dart · Dockerfile · Elixir · HTML · Go · Java · JavaScript · JSX · JSON · Julia · Jsonnet · Kotlin · Lisp · Lua · OCaml · PHP · Python · R · Ruby · Rust · Scala · Scheme · Solidity · Swift · Terraform · TypeScript · TSX · YAML · XML · Generic (ERB, Jinja, etc.)

**Semgrep Supply Chain** supports 12 languages across 15 package managers, including:

C# (NuGet) · Dart (Pub) · Go (Go modules, `go mod`) · Java (Gradle, Maven) · Javascript/Typescript (npm, Yarn, Yarn 2, Yarn 3, pnpm) · Kotlin (Gradle, Maven) · PHP (Composer) · Python (pip, pip-tool, Pipenv, Poetry) · Ruby (RubyGems) · Rust (Cargo) · Scala (Maven) · Swift (SwiftPM)

For more information, see [Supported languages](https://semgrep.dev/docs/supported-languages/).

### Getting started 🚀

1. [From the Semgrep AppSec Platform](#option-1-getting-started-from-the-semgrep-appsec-platform-recommended)
2. [From the CLI](#option-2-getting-started-from-the-cli)

For new users, we recommend starting with the [Semgrep AppSec Platform](#option-1-getting-started-from-the-semgrep-appsec-platform-recommended) because it provides a visual interface, a demo project, result triaging and exploration workflows, and makes setup in CI/CD fast. Scans are still local and code isn't uploaded. Alternatively, you can also start with the CLI and navigate the terminal output to run one-off searches.

### Option 1: Getting started from the Semgrep Appsec Platform (Recommended)

<a href="https://go.semgrep.dev/login-ghrmgo"  target="_blank"><img src="https://raw.githubusercontent.com/semgrep/semgrep/develop/images/semgrep-main-image.jpg" alt="Semgrep platform image"/> </a>

1.  Register on <a href="https://go.semgrep.dev/login-ghrmgo" target="_blank">semgrep.dev</a>

2.  Explore the demo findings to learn how Semgrep works

3.  Scan your project by navigating to `Projects > Scan New Project > Run scan in CI`

4.  Select your version control system and follow the onboarding steps to add your project. After this setup, Semgrep will scan your project after every pull request.

5.  [Optional] If you want to run Semgrep locally, follow the steps in the CLI section.

### Notes:

If there are any issues, <a href="https://go.semgrep.dev/slack" target="_blank">please ask for help in the Semgrep Slack</a>.

### Option 2: Getting started from the CLI

1.  Install Semgrep CLI

```
# For macOS
$ brew install semgrep

# For Ubuntu/WSL/Linux/macOS
$ python3 -m pip install semgrep

# To try Semgrep without installation run via Docker
$ docker run -it -v "${PWD}:/src" semgrep/semgrep semgrep login
$ docker run -e SEMGREP_APP_TOKEN=<TOKEN> --rm -v "${PWD}:/src" semgrep/semgrep semgrep ci
```

2.  Run `semgrep login` to create your account and login to Semgrep.

Logging into Semgrep gets you access to:

- [Semgrep Supply Chain](https://semgrep.dev/products/semgrep-supply-chain): A dependency scanner that detects reachable vulnerabilities in third party libraries
- [Semgrep Code's Pro rules](https://semgrep.dev/products/semgrep-code): 600+ high confidence rules written by Semgrep's security research team
- [Semgrep Code's Pro engine](https://semgrep.dev/products/pro-engine/): An advanced code analysis engine, designed to detect complex vulnerabilities, and reduce false positives

3.  Go to your app's root directory and run `semgrep ci`. This will scan your project to check for vulnerabilities in your source code and its dependencies.

4.  Try writing your own query interactively with `-e`. For example, a check for Python == where the left and right hand sides are the same (potentially a bug):
    `$ semgrep -e '$X == $X' --lang=py path/to/src`

### Semgrep Ecosystem

The Semgrep ecosystem includes the following products:

- [Semgrep Code](https://semgrep.dev/products/semgrep-code) - Scan your code with Semgrep's proprietary rules (written by our Security Research team) using our cross-file and cross-function analysis. Designed to find OWASP Top 10 vulnerabilities and protect against critical security risks. Semgrep Code is available on both [free and paid tiers](https://semgrep.dev/pricing).
- [Semgrep Supply Chain (SSC)](https://semgrep.dev/products/semgrep-supply-chain) - A high-signal dependency scanner that detects reachable vulnerabilities in open source third-party libraries and functions across the software development life cycle (SDLC). Semgrep Supply Chain is available on both [free and paid tiers](https://semgrep.dev/pricing).
- [Semgrep Secrets [NEW!]](https://semgrep.dev/blog/2023/introducing-semgrep-secrets/) - Secrets detection that uses semantic analysis, improved entropy analysis, and validation together to accurately detect sensitive credentials in developer workflows. [Book a demo](https://get.semgrep.dev/secrets-beta-request.html) to request early access to the product.
- [Semgrep AppSec Platform](https://semgrep.dev/login) - Deploy, manage, and monitor Semgrep at scale, with [free and paid tiers](https://semgrep.dev/pricing). Integrates with continuous integration (CI) providers such as GitHub, GitLab, CircleCI, and more.
- [Semgrep OSS Engine](https://semgrep.dev/docs/cli-reference/) - The open-source engine and community-contributed rules at the heart of everything (this project).

To learn more about Semgrep, visit:

- [Semgrep Playground](https://semgrep.dev/editor) - An online interactive tool for writing and sharing rules.
- [Semgrep Registry](https://semgrep.dev/explore) - 2,000+ community-driven rules covering security, correctness, and dependency vulnerabilities.

Join hundreds of thousands of other developers and security engineers already using Semgrep at companies like GitLab, Dropbox, Slack, Figma, Shopify, HashiCorp, Snowflake, and Trail of Bits.

Semgrep is developed and commercially supported by [Semgrep, Inc., a software security company](https://semgrep.dev).

### Semgrep Rules

Semgrep rules look like the code you already write; no abstract syntax trees, regex wrestling, or painful DSLs. Here's a quick rule for finding Python `print()` statements.

Run it online in Semgrep’s Playground by [clicking here](https://semgrep.dev/s/ievans:print-to-logger).

<p align="center">
    <a href="https://semgrep.dev/s/ievans:print-to-logger"  target="_blank"><img src="https://raw.githubusercontent.com/semgrep/semgrep/develop/images/semgrep-example-rules-editor.jpg" width="582" alt="Semgrep rule example for finding Python print() statements" /></a>
</p>

#### Examples

Visit [Docs > Rule examples](https://semgrep.dev/docs/writing-rules/rule-ideas/) for use cases and ideas.

| Use case                          | Semgrep rule                                                                                                                                                                                                                                                                                                                                                                                                |
| :-------------------------------- | :---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Ban dangerous APIs                | [Prevent use of exec](https://semgrep.dev/playground/s/lglB)                                                                                                                                                                                                                                                                                                                                                |
| Search routes and authentication  | [Extract Spring routes](https://semgrep.dev/playground/s/Y6wD)                                                                                                                                                                                                                                                                                                                                              |
| Enforce the use secure defaults   | [Securely set Flask cookies](https://semgrep.dev/playground/s/6KwW)                                                                                                                                                                                                                                                                                                                                         |
| Tainted data flowing into sinks   | [ExpressJS dataflow into sandbox.run](https://semgrep.dev/playground/s/qEpR)                                                                                                                                                                                                                                                                                                                                |
| Enforce project best-practices    | [Use assertEqual for == checks](https://semgrep.dev/playground/s/oEox), [Always check subprocess calls](https://semgrep.dev/playground/s/zENk)                                                                                                                                                                                                                                                              |
| Codify project-specific knowledge | [Verify transactions before making them](https://semgrep.dev/playground/s/p8zk)                                                                                                                                                                                                                                                                                                                             |
| Audit security hotspots           | [Finding XSS in Apache Airflow](https://semgrep.dev/playground/s/KPwj), [Hardcoded credentials](https://semgrep.dev/playground/s/2Br8)                                                                                                                                                                                                                                                                      |
| Audit configuration files         | [Find S3 ARN uses](https://semgrep.dev/playground/s/jEKD)                                                                                                                                                                                                                                                                                                                                                   |
| Migrate from deprecated APIs      | [DES is deprecated](https://semgrep.dev/playground/r/java.lang.security.audit.crypto.des-is-deprecated.des-is-deprecated), [Deprecated Flask APIs](https://semgrep.dev/playground/r/python.flask.maintainability.deprecated.deprecated-apis.flask-deprecated-apis), [Deprecated Bokeh APIs](https://semgrep.dev/playground/r/python.bokeh.maintainability.deprecated.deprecated_apis.bokeh-deprecated-apis) |
| Apply automatic fixes             | [Use listenAndServeTLS](https://semgrep.dev/playground/s/1Ayk)                                                                                                                                                                                                                                                                                                                                              |

### Extensions

Visit [Docs > Extensions](https://semgrep.dev/docs/extensions/) to learn about using Semgrep in your editor or pre-commit. When integrated into CI and configured to scan pull requests, Semgrep will only report issues introduced by that pull request; this lets you start using Semgrep without fixing or ignoring pre-existing issues!

### Documentation

Browse the full Semgrep [documentation on the website](https://semgrep.dev/docs). If you’re new to Semgrep, check out [Docs > Getting started](https://semgrep.dev/docs/getting-started/) or the [interactive tutorial](https://semgrep.dev/learn).

### Metrics

Using remote configuration from the [Registry](https://semgrep.dev/r) (like `--config=p/ci`) reports pseudonymous rule metrics to semgrep.dev.

Using configs from local files (like `--config=xyz.yml`) does **not** enable metrics.

To disable Registry rule metrics, use `--metrics=off`.

The Semgrep [privacy policy](https://semgrep.dev/docs/metrics) describes the principles that guide data-collection decisions and the breakdown of the data that are and are not collected when the metrics are enabled.

### More

- [Frequently asked questions (FAQs)](https://semgrep.dev/docs/faq/)
- [Contributing](https://semgrep.dev/docs/contributing/contributing/)
- [Build instructions for developers](INSTALL.md)
- [Ask questions in the Semgrep community Slack](https://go.semgrep.dev/slack)
- [CLI reference and exit codes](https://semgrep.dev/docs/cli-usage)
- [Semgrep YouTube channel](https://www.youtube.com/c/semgrep)
- [License (LGPL-2.1)](LICENSE)
- [Licensing Semgrep](https://semgrep.dev/docs/licensing/)

### Upgrading

To upgrade, run the command below associated with how you installed Semgrep:

```sh
# Using Homebrew
$ brew upgrade semgrep

# Using pip
$ python3 -m pip install --upgrade semgrep

# Using Docker
$ docker pull semgrep/semgrep:latest
```
