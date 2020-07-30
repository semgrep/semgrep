<p align="center">
    <img src="semgrep.svg" height="100" alt="Semgrep logo"/>
</p>
<h3 align="center">
  Lightweight static analysis for many languages.
  </br>
  Find and block bug variants with rules that look like source code.
</h3>

<p align="center">
  <a href="#getting-started">Getting Started</a>
  <span> · </span>
  <a href="#design-choices">Design Choices</a>
  <span> · </span>
  <a href="#use-cases">Use Cases</a>
  <br/>
  <a href="#resources">Resources</a>
  <span> · </span>
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
    <img src="https://img.shields.io/github/stars/returntocorp/semgrep?label=GitHub%20Stars&style=flat-square" alt="1000+ GitHub stars" />
  </a>
</p>

Semgrep tl;dr:

* Semgrep is a customizable, lightweight, static analysis tool for finding bugs
* Can run in CI, pre-commit, or in the editor
* Batteries included with [hundreds of existing community rules]()
* Combine the speed + customization of grep with the expressiveness of traditional/heavyweight static analysis tools
* Runs offline, on uncompiled code, fast and open source!
* No painful DSL, patterns look like the source code you’re targeting

Semgrep supports:

| **Python** | **Java** | **Go** | **JavaScript** | **C** | **Ruby** | **TypeScript** | **PHP**   |
| :--------- | :------- | :----- | :------------- | :---- | :------- | :------------- | :-------- |
| ✅         | ✅       | ✅     | ✅             | ✅    | 🚧       | Coming...      | Coming... |

Semgrep is proudly supported by r2c. Learn more about a hosted version of Semgrep with an enterprise feature set at [r2c.dev]().

## Getting Started

The best place to start with Semgrep is with its online tour: [semgrep.live/tour](). If you skip the tour, [Use Cases]() will give you a high level sense of Semgrep's applications for DevSecOps.

Semgrep can be installed using `brew`, `pip`, or `docker`:

```sh
# For macOS:
$ brew install semgrep

# On Ubuntu/WSL/linux, we recommend installing via `pip`
$ pip3 install semgrep

# To try Semgrep without installation run via Docker
$ docker run --rm -v "${PWD}:/src" returntocorp/semgrep --help
```

Once installed, Semgrep can be run with quick patterns or entire rule packs:

```sh
# Check for Python == where the left and right hand sides are the same (often a bug)
$ semgrep -e `$X==$X` --lang=py path/to/src

# Run the default rule pack with rules for many languages
$ semgrep --config=default path/to/src
```

To learn more about Semgrep's rule syntax visit [TODO]().

To explore community rule packs and learn about CI integrations visit [TODO]()

## Design Choices

Semgrep is optimized for:

- **Speed**: Fast enough to run on every build, commit, or file save
- **Finding bugs that matter**: Run your rules or community rule packs from the [Semgrep Registry](_https://semgrep.live/packs). Rules match source code at the Abstract Syntax Tree (AST) level, unlike regexes that match strings and aren't semantically aware.
- **Ease of customization**: Rules look like the code you’re searching, no static analysis PhD required. They don't require compiled code, only source, reducing iteration time.
- **Ease of integration**. Highly portable and many CI and git-hook integrations already exist. Output `--json` and pipe results into your existing systems.
- **Polyglot environments**: Don't learn and maintain multiple tools for your polyglot environment (e.g. ESLint, find-sec-bugs, RuboCop, Gosec). Use the same syntax and concepts independent of language.

Semgrep emphasis on speed has led to its focus on per-file analysis. As a result, Semgrep doesn't support complex interprocedural dataflows, which are time intensive and costly to compute.

TODO - GOOGLE PAPER, INSTAGRAM GRAPHIC

## Use Cases

Search your code

- Vulnerabilities
- Audit security hotspots
- Extract routes
- Codify domain knowledge

Guard your code

- Secure defaults
- Banned APIs
- Best- and required- practices
- Configuration file auditing

Upgrade your code

- Migrate from deprecated APIs
- Apply automatic fixes

Watch your code

- See fixes over time

## Resources

Learn more:

- [Semgrep presentation](https://www.youtube.com/watch?v=pul1bRIOYc8) and [slides](https://web-assets.r2c.dev/presentations/r2c-semgrep-OWASP-BayArea-21-May-2020.pdf) from the Bay Area OWASP meetup.
- Check out the [r2c YouTube channel](https://www.youtube.com/channel/UC5ahcFBorwzUTqPipFhjkWg) for more videos.
- More detailed [Semgrep docs](docs/README.md)

Get in touch:

- Submit a [bug report](https://github.com/returntocorp/semgrep/issues)
- Join our [community Slack](https://join.slack.com/t/r2c-community/shared_invite/enQtNjU0NDYzMjAwODY4LWE3NTg1MGNhYTAwMzk5ZGRhMjQ2MzVhNGJiZjI1ZWQ0NjQ2YWI4ZGY3OGViMGJjNzA4ODQ3MjEzOWExNjZlNTA) to say "hi" or ask questions

## Usage

### Command Line Options

```shell
$ semgrep --help

positional arguments:
  target                Search these files or directories.
                        Defaults to  entire current working directory.
                        Implied argument if piping to semgrep.

optional arguments:
  -h, --help            show this help message and exit
  --exclude EXCLUDE     Skip any file with this name; --exclude='*.py'
                        will ignore foo.py as well as src/foo.py.
                        Can add multiple times. Overrides includes.
  --include INCLUDE     Scan only files with this name,
                        such as --include='*.jsx'. Can add multiple times.
  --version             Show the version and exit.

config:
  -f CONFIG, --config CONFIG
                        YAML configuration file, directory of YAML files
                        ending in .yml|.yaml, URL of a configuration file,
                        or semgrep registry entry name. See README for
                        information on configuration file format.
  -e PATTERN, --pattern PATTERN
                        Code search pattern. See README for information
                        on pattern features.
  -l LANG, --lang LANG  Parse pattern and all files in specified language.
                        Must be used with -e/--pattern.
  --validate            Validate configuration file(s). No search is performed.
  --strict              Only invoke semgrep if configuration files(s) are valid.
  --dangerously-allow-arbitrary-code-execution-from-rules
                        WARNING: allow rules to run arbitrary code.
                        ONLY ENABLE IF YOU TRUST THE SOURCE OF ALL RULES IN YOUR CONFIGURATION.
  -j JOBS, --jobs JOBS  Number of subprocesses to use to run checks in parallel.
                        Defaults to the number of CPUs on the system.

output:
  -q, --quiet           Do not print anything to stdout. Results can be
                        saved to an output file via -o/--output. Exit
                        code provides success status.
  --no-rewrite-rule-ids
                        Do not rewrite rule ids when they appear in nested
                        sub-directories (by default, rule 'foo' in
                        test/rules.yaml will be renamed 'test.foo').
  -o OUTPUT, --output OUTPUT
                        Save search results to a file or post to URL.
                        Default is to print to stdout.
  --json                Output results in JSON format.
  --debugging-json      Output JSON with extra debugging information.
  --sarif               Output results in SARIF format.
  --test                Run test suite.
  --dump-ast            Show AST of the input file or passed expression and
                        then exit (can use --json).
  --error               Exit 1 if there are findings. Useful for CI and scripts.
  -a, --autofix         Apply the autofix patches. WARNING: data loss can
                        occur with this flag. Make sure your files are stored
                        in a version control system.

logging:
  -v, --verbose         Set the logging level to verbose.
                        E.g. statements about which files are being processed
                        will be printed.
```

### Exit Codes

`semgrep` may exit with the following exit codes:

- `0`: Semgrep ran successfully and found no errors
- `1`: Semgrep ran successfully and found issues in your code
- \>=`2`: Semgrep failed to run

## Contributing

Semgrep is LGPL-licensed, feel free to help out: [CONTRIBUTING](https://github.com/returntocorp/semgrep/blob/develop/CONTRIBUTING.md).

Semgrep is a frontend to a larger program analysis library named [`pfff`](https://github.com/returntocorp/pfff/). `pfff` began and was open-sourced at [Facebook](https://github.com/facebookarchive/pfff) but is now archived. The primary maintainer now works at [r2c](https://r2c.dev). Semgrep was originally named `sgrep` and was renamed to avoid collisons with existing projects.

## Commercial Support

Semgrep is proudly supported by [r2c](https://r2c.dev). We're hiring!

Interested in a fully-supported, hosted version of semgrep? [Drop your email](https://forms.gle/dpUUvSo1WtELL8DW6) and we'll be in touch!
