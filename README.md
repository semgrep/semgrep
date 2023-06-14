<br />
<p align="center">
  <a href="https://semgrep.dev">
    <picture>
      <source media="(prefers-color-scheme: light)" srcset="images/semgrep-logo-light.svg">
      <source media="(prefers-color-scheme: dark)" srcset="images/semgrep-logo-dark.svg">
      <img src="https://raw.githubusercontent.com/returntocorp/semgrep/develop/images/semgrep-logo-light.svg" height="100" alt="Semgrep logo"/>
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
    <img src="https://img.shields.io/badge/slack-2.5k%20members-green?style=flat-square" alt="Join Semgrep community Slack" />
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
  <a href="https://twitter.com/intent/follow?screen_name=semgrep">
    <img src="https://img.shields.io/twitter/follow/semgrep?label=Follow%20semgrep&style=social&color=blue" alt="Follow @semgrep on Twitter" />
  </a>
</p>
</br>

Semgrep is a fast, open-source, static analysis engine for finding bugs, detecting vulnerabilities in third-party dependencies, and enforcing code standards. Semgrep analyzes code locally on your computer or in your build environment: **code is never uploaded**. [Get started →.](#getting-started-)

<a href="#option-1-getting-started-from-the-cli">
<img src="https://raw.githubusercontent.com/returntocorp/semgrep/develop/images/semgrep-scan-cli.jpg" alt="Semgrep CLI image"/></a>

### Language support

Semgrep supports 30+ languages.

| Category     | Languages                                                                                                                                                                     |
| ------------ | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| GA           | C# · Go · Java · JavaScript · JSX · JSON · PHP · Python · Ruby · Scala · Terraform · TypeScript · TSX                                                                         |
| Beta         | Kotlin · Rust                                                                                                                                                                 |
| Experimental | Bash · C · C++ · Clojure · Dart · Dockerfile · Elixir · HTML · Julia · Jsonnet · Lisp · Lua · OCaml · R · Scheme · Solidity · Swift · YAML · XML · Generic (ERB, Jinja, etc.) |

### Getting started 🚀

1. [From the CLI](#option-1-getting-started-from-the-cli)
2. [From the Semgrep Cloud Platform](#option-2-getting-started-from-the-semgrep-cloud-platform-recommended)

For beginners, we recommend starting with the [Semgrep Cloud Platform](#option-2-getting-started-from-the-semgrep-cloud-platform-recommended) because it provides a visual interface, a demo project, result triaging and exploration workflows, and makes setup in CI/CD fast. Scans are still local and code isn't uploaded. Alternatively, you can also start with the CLI without logging in and navigate the terminal output to run one-off searches.

### Option 1: Getting started from the CLI

1.  Install Semgrep CLI

```
# For macOS
$ brew install semgrep

# For Ubuntu/WSL/Linux/macOS
$ python3 -m pip install semgrep

# To try Semgrep without installation run via Docker
$ docker run --rm -v "${PWD}:/src" returntocorp/semgrep semgrep
```

2.  Go to your app's root directory and run `semgrep scan --config auto`. This will scan your project with the default settings.

3.  [Optional, but recommended] Run `semgrep login` to get the login URL for the Semgrep Cloud Platform. Open the login URL in the browser and login.

### Option 2: Getting started from the Semgrep Cloud Platform (Recommended)

<a href="https://go.semgrep.dev/login-ghrmgo"  target="_blank"><img src="https://raw.githubusercontent.com/returntocorp/semgrep/develop/images/semgrep-main-image.jpg" alt="Semgrep platform image"/> </a>

1.  Register to <a href="https://go.semgrep.dev/login-ghrmgo" target="_blank">semgrep.dev</a>

2.  Explore the demo app

3.  Scan your project by navigating to `Projects > Scan New Project > Run scan in CI`

4.  Select your version control system and follow the wizard to add your project. After this setup, Semgrep will scan your project after every pull request.

5.  [Optional but recommended] If you want to run Semgrep locally, follow the steps in the CLI section.

### Notes:

1.  Visit [Docs > Running rules](https://semgrep.dev/docs/running-rules/) to learn more about `auto` config and other rules.

2.  If there are any issues, please ask in the Smegrep Slack group <a href="https://go.semgrep.dev/slack" target="_blank"> https://go.semgrep.dev/slack</a>

3.  To run Semgrep Supply Chain, [contact the Semgrep team](https://semgrep.dev/contact-us).
    Visit the [full documentation](https://semgrep.dev/docs/getting-started/) to learn more.

### Semgrep Ecosystem

The Semgrep ecosystem includes the following products:

- Semgrep OSS Engine - The open-source engine at the heart of everything (this project).
- [Semgrep Cloud Platform (SCP)](https://semgrep.dev/login) - Deploy, manage, and monitor SAST and SCA at scale using Semgrep, with [free and paid tiers](https://semgrep.dev/pricing). Integrates with continuous integration (CI) providers such as GitHub, GitLab, CircleCI, and more.
- [Semgrep Code](https://semgrep.dev/products/semgrep-code) - Scan your code with Semgrep's Pro rules and Semgrep Pro Engine to find OWASP Top 10 vulnerabilities and protect against critical security risks specific to your organization. Semgrep Code provides both Community (free) and Team (paid) tiers.
- [Semgrep Supply Chain (SSC)](https://semgrep.dev/products/semgrep-supply-chain) - A high-signal dependency scanner that detects reachable vulnerabilities in open source third-party libraries and functions across the software development life cycle (SDLC). Semgrep Supply Chain is available on Team (paid) tiers.

and:

- [Semgrep Playground](https://semgrep.dev/editor) - An online interactive tool for writing and sharing rules.
- [Semgrep Registry](https://semgrep.dev/explore) - 2,000+ community-driven rules covering security, correctness, and dependency vulnerabilities.

Join hundreds of thousands of other developers and security engineers already using Semgrep at companies like GitLab, Dropbox, Slack, Figma, Shopify, HashiCorp, Snowflake, and Trail of Bits.

Semgrep is developed and commercially supported by [Semgrep, Inc., a software security company](https://semgrep.dev).

### Semgrep Rules

Semgrep rules look like the code you already write; no abstract syntax trees, regex wrestling, or painful DSLs. Here's a quick rule for finding Python `print()` statements.

Run it online in Semgrep’s Playground by [clicking here](https://semgrep.dev/s/ievans:print-to-logger).

<p align="center">
    <a href="https://semgrep.dev/s/ievans:print-to-logger"  target="_blank"><img src="https://raw.githubusercontent.com/returntocorp/semgrep/develop/images/semgrep-example-rules-editor.jpg" width="582" alt="Semgrep rule example for finding Python print() statements" /></a>
</p>

#### Examples

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
