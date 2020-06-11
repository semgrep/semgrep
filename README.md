<p align="center">
    <img src="semgrep.svg" height="100" alt="Semgrep logo"/>
</p>
<h3 align="center">
  Lightweight static analysis for many languages.
  </br>
  Find and block bug variants with rules that look like source code.
</h3>

<p align="center">
  <a href="#installation">Installation</a>
  <span> · </span>
  <a href="#motivation">Motivation</a>
  <span> · </span>
  <a href="#overview">Overview</a>
  <span> · </span>
  <a href="#usage">Usage</a>
  <br/>
  <a href="#resources">Resources</a>
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
  <a href="https://twitter.com/intent/follow?screen_name=r2cdev">
    <img src="https://img.shields.io/twitter/follow/r2cdev?label=Follow%20r2cdev&style=social&color=blue" alt="Follow @r2cdev" />
  </a>
</p>

Semgrep is a [command-line](#installation) tool for offline static analysis. Use pre-built or custom rules to enforce code and security standards in your codebase. You can try it now with our [interactive live editor](https://semgrep.live/).

Semgrep combines the convenient and iterative style of `grep` with the powerful features of an Abstract Syntax Tree (AST) matcher and limited dataflow. Easily find function calls, class or method definitions, and more without having to understand ASTs or wrestle with regexes.

Visit [Installation](#installation) and [Usage](#usage) to get started.

## Installation
> Want to skip installation? You can run Semgrep via a live editor at [semgrep.live](https://semgrep.live/).

On macOS, binaries are available via Homebrew:

```bash
$ brew install semgrep
```

On Ubuntu, an install script is available with each [release](https://github.com/returntocorp/semgrep/releases/download/v0.10.1/semgrep-v0.10.1-ubuntu-generic.sh)

```bash
$ ./semgrep-v0.10.1-ubuntu-generic.sh
```

To try Semgrep without installation, you can also run it via Docker:

```
$ docker run --rm -v "${PWD}:/home/repo" returntocorp/semgrep --help
``` 

See [Usage](#usage) to learn about running pre-built rules and writing custom ones.

## Motivation

Semgrep exists because:

1. Insecure code is easy to write
2. The future of security involves automatically guiding developers towards a “paved road” made of default-safe frameworks (i.e. [React](https://reactjs.org/) or Object-relational Mappers)
3. `grep` isn’t expressive enough and traditional static analysis tools (SAST) are too complicated/slow for paved road automation

The AppSec, Developer, and DevOps communities deserves a static analysis tool that is fast, easy to use, code-aware, multi-lingual, and open source!

## Overview

Semgrep is optimized for:

* **Speed**: Fast enough to run on every build, commit, or file save
* **Finding bugs that matter**: Run your own specialized rules or choose OWASP 10 checks from the [Semgrep Registry](https://semgrep.live/r). Rules match source code at the Abstract Syntax Tree (AST) level, unlike regexes that match strings and aren't semantically aware.
* **Ease of customization**: Rules look like the code you’re searching, no static analysis PhD required. They don't require compiled code, only source, reducing iteration time.
* **Ease of integration**. Highly portable and many CI and git-hook integrations already exist. Output `--json` and pipe results into your existing systems.
* **Polyglot environments**: Don't learn and maintain multiple tools for your polyglot environment (e.g. ESLint, find-sec-bugs, RuboCop, Gosec). Use the same syntax and concepts independent of language.


### Language Support

| **Python** | **JavaScript** | **Go &nbsp; &nbsp; &nbsp;** | **Java &nbsp;** | **C &nbsp; &nbsp; &nbsp; &nbsp;** | **Ruby** | **TypeScript** | **PHP &nbsp; &nbsp;** |
|:-----------|:---------------|:----------------------------|:----------------|:----------------------------------|:---------|:---------------|:----------------------|
| ✅          | ✅              | ✅                           | ✅               | ✅                                 | 🚧 |  Coming...      | Coming...             |

Missing support for a language? Let us know by filing a ticket, joining our Slack, or emailing [support@r2c.dev](mailto:support@rc2.dev?subject=Language%20Support:).

### Pattern Syntax Teaser

One of the most unique and useful things about Semgrep is how easy it is to write and iterate on queries.

The goal is to make it as *easy as possible* to go from an idea in your head to finding the code patterns you intend to.

**Example**: Say you want to find all calls to a function named `exec`, and you don't care about the arguments. With Semgrep, you could simply supply the pattern `exec(...)` and you'd match:

~~~python
# Simple cases grep finds
exec("ls")
exec(some_var)

# But you don't have to worry about whitespace
exec (foo)

# Or calls across multiple lines
exec (
    bar
)
~~~

Importantly, Semgrep would *not* match the following:

~~~python
# grep would match this, but Semgrep ignores it because
# it doesn't have the right function name
other_exec(bar)

# Semgrep ignores commented out lines
# exec(foo)

# and hard-coded strings
print("exec(bar)")
~~~

Semgrep will even match aliased imports:

~~~python
# Semgrep knows that safe_function refers to exec so it
# will still match!
#   Oof, try finding this with grep
import exec as safe_function
safe_function(tricksy)
~~~

Play with this example in your browser [here](https://semgrep.live/QrkD), or copy the above code into a file locally (`exec.py`) and run:

~~~python
$ semgrep -l python -e "exec(...)" /path/to/exec.py
~~~

More example patterns:

| **Pattern**                                                        | **Matches**                                                |
|:-------------------------------------------------------------------|:-----------------------------------------------------------|
| [`$X == $X`](https://semgrep.live/20B)                             | `if (node.id == node.id): ...`                             |
| [`requests.get(..., verify=False, ...)`](https://semgrep.live/jqn) | `requests.get(url, timeout=3, verify=False)`               |
| [`os.system(...)`](https://semgrep.live/1W5)                       | `from os import system; system('echo semgrep')`            |
| [`$ELEMENT.innerHTML`](https://semgrep.live/9ze)                   | ``el.innerHTML = "<img src='x' onerror='alert(`XSS`)'>";`` |
| [`$TOKEN.SignedString([]byte("..."))`](https://semgrep.live/rXW)   | `ss, err := token.SignedString([]byte("HARDCODED KEY"))`   |

→ [see more example patterns in the Semgrep Registry](https://semgrep.live/registry).

For more info on what you can do in patterns, see the [pattern features
docs](docs/pattern-features.md).

## Usage

Semgrep supports three primary workflows:

* Run pre-built rules
* Writing custom rules
* Run Semgrep continously in CI

The following sections cover each in more detail.

### Run Pre-Built Rules

The easiest way to get started with Semgrep (other than [semgrep.live](https://semgrep.live/)) is to scan your code with pre-built rules.

The [Semgrep Registry](https://semgrep.live/r) contains rules for many programming errors, including security issues and correctness bugs. Security rules are annotated with CWE and OWASP metadata when applicable. OWASP rule coverage per language is displayed below.

<p align="center">
    <img width="600" src="https://web-assets.r2c.dev/semgrep-rules-owasp-coverage-20200520.png" style="max-width:100%;" />
</p>

You can use pre-built [Rule Packs](https://semgrep.live/packs), that contain sets of rules grouped by language and/or framework:

```bash
$ semgrep --config=https://semgrep.live/c/p/java
$ semgrep --config=https://semgrep.live/c/p/python
$ semgrep --config=https://semgrep.live/c/p/golang
$ semgrep --config=https://semgrep.live/c/p/javascript
...
```

Or you can run all of Semgrep's default rules for all languages as appropriate (note: each rule says what language it's for, so Semgrep won't try to run a Python rule on Java code).

```bash
$ semgrep --config=r2c
```

You can also run a specific rule or group of rules:

```bash
# Run a specific rule
$ semgrep --config=https://semgrep.live/c/r/java.spring.security.audit.cookie-missing-samesite

# Run a set of rules
$ semgrep --config=https://semgrep.live/c/r/java.spring.security
```

All public Semgrep rules can be viewed on the [Registry](https://semgrep.live/r), which pulls the rules from YAML files defined in the [semgrep-rules](https://github.com/returntocorp/semgrep-rules) GitHub repo.

Here are some sample vulnerable repos to test on:
* Django: [lets-be-bad-guys](https://github.com/mpirnat/lets-be-bad-guys), [django.nV](https://github.com/nVisium/django.nV)
* Flask: [Vulnerable-Flask-App](https://github.com/we45/Vulnerable-Flask-App)
* Java: [WebGoat](https://github.com/WebGoat/WebGoat), [OWASP Benchmark](https://github.com/OWASP/Benchmark)
* NodeJS: [OWASP Juice Shop](https://github.com/bkimminich/juice-shop), [DevSlop Pixi](https://github.com/DevSlop/Pixi)
* Golang: [GoVWA](https://github.com/0c34/govwa)

### Writing Custom Rules

One of the strengths of Semgrep is how easy it is to write rules.

This makes it possible to:
* Quickly port rules from other tools.
* Think of an interesting code pattern, and then find instances of it in your
    code.
* Find code base or org-specific bugs and antipatterns - things that built-in
    checks for existing tools won't find because they're unique to you.
* and more!

#### Simple Rules

For iterating on simple patterns, you can use the `--lang` and `--pattern`
flags.

```bash
$ semgrep --lang javascript --pattern 'eval(...)' path/to/file.js
```

The `--lang` flag tells Semgrep which language you're targeting and `--pattern` is the code pattern to search for.

#### Advanced Rules

Some rules need more than one line of pattern to express. Sometimes you want to express code patterns, like: `X` must be true AND `Y` must be too, or `X` but NOT `Y`, or `X` must occur inside a block of code that `Y` matches.

For these cases, Semgrep has a more powerful and flexible [YAML syntax](docs/configuration-files.md).

You can run a single rule or directory of rules specified in YAML by:
```bash
$ semgrep --config my_rule.yml path/to/dir_or_file

$ semgrep --config yaml_dir/ path/to/dir_or_file
```

**Example Advanced Rule**

Say you are building a financial trading application in which every `Transaction` object must first be passed to `verify_transaction()` before being passed to `make_transaction()`, or it's a business logic bug.

You can express this behavior with the following Semgrep YAML pattern:

```yaml
rules:
- id: find-unverified-transactions
  patterns:
    - pattern: |
        public $RETURN $METHOD(...){
            ...
            make_transaction($T);
            ...
        }
    - pattern-not: |
        public $RETURN $METHOD(...){
            ...
            verify_transaction($T);
            ...
            make_transaction($T);
            ...
        }
  message: |
    In $METHOD, there's a call to make_transaction() without first calling verify_transaction() on the Transaction object.
```

* `$RETURN`, `$METHOD`, and `$T` are *metavariables*, an abstraction that Semgrep provides when you want to match something but you don't know exactly what it is ahead of time.
  * You can think of *metavariables* like a [capture group](https://regexone.com/lesson/capturing_groups) in regular expressions.
* The `pattern` clause defines what we're looking for: any method that calls `make_transaction()`.
* The `pattern-not` clause *filters out* matches we don't want; in this case, methods where a transaction (`$T`) is passed to `verify_transaction()` before `make_transaction()`.
* The `message` is what's returned in Semgrep output, either to STDOUT or as a comment on the pull request on GitHub or other systems.
  * Note that *metavariables* can be used to customize messages and make them
    contextually relevant. Here we're helpfully telling the user the method
    where we've identified the bug.

You can play with this transaction example here: https://semgrep.live/4b4g.

**Learn More**

* See the [pattern features docs](docs/pattern-features.md) for more info and
  examples on the flexibility and power of Semgrep patterns.
* See the [YAML configuration file docs](docs/configuration-files.md) for
  details on all of the keys that can be used and how they work.

### Run Semgrep Continously in CI

Semgrep can be run via CLI or Docker and output results as JSON (via the `--json` flag), so it can be inserted into any CI pipeline and have its results processed by whatever tools you're using.

Semgrep is aware of *diffs*, so it can report only findings that occur in newly added code, for example, in a commit or pull request.

Currently, the easiest way to integrate Semgrep into CI is via a GitHub action we've built. See the [integrations docs](docs/integrations.md) for more details.

Semgrep can also output results in the standardized Static Analysis Results Interchange Format ([SARIF](https://docs.oasis-open.org/sarif/sarif/v2.1.0/cs01/sarif-v2.1.0-cs01.html)) with the `--sarif` flag, if you use tools that accept this format.

## Resources

Learn more:
* [Semgrep presentation](https://www.youtube.com/watch?v=pul1bRIOYc8) and [slides](https://web-assets.r2c.dev/presentations/r2c-semgrep-OWASP-BayArea-21-May-2020.pdf) from the Bay Area OWASP meetup. 
* Check out the [r2c YouTube channel](https://www.youtube.com/channel/UC5ahcFBorwzUTqPipFhjkWg) for more videos.
* More detailed [Semgrep docs](docs/README.md)

Get in touch:
* Submit a [bug report](https://github.com/returntocorp/semgrep/issues)
* Join our [community Slack](https://join.slack.com/t/r2c-community/shared_invite/enQtNjU0NDYzMjAwODY4LWE3NTg1MGNhYTAwMzk5ZGRhMjQ2MzVhNGJiZjI1ZWQ0NjQ2YWI4ZGY3OGViMGJjNzA4ODQ3MjEzOWExNjZlNTA) to say "hi" or ask questions

## Contributing

Semgrep is LGPL-licensed, feel free to help out: [CONTRIBUTING](https://github.com/returntocorp/semgrep/blob/develop/CONTRIBUTING.md).

Semgrep is a frontend to a larger program analysis library named [`pfff`](https://github.com/returntocorp/pfff/). `pfff` began and was open-sourced at [Facebook](https://github.com/facebookarchive/pfff) but is now archived. The primary maintainer now works at [r2c](https://r2c.dev). Semgrep was originally named `sgrep` and was renamed to avoid collisons with existing projects.

## Commercial Support

Semgrep is proudly supported by [r2c](https://r2c.dev). We're hiring!

Interested in a fully-supported, hosted version of semgrep? [Drop your email](https://forms.gle/dpUUvSo1WtELL8DW6) and we'll ping you!
