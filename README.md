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
  <a href="#usage">Usage</a>
  <span> · </span>
  <a href="#overview">Overview</a>
  <span> · </span>
  <a href="#workflows">Workflows</a>
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

Semgrep is an open source (LGPL) static analysis tool for modern dev environments. It's used to enforce meaningful code and security standards ("the paved road") at file-save, commit, and build time. You can [write your own rules]() or run [community rule packs](). Either way, your code is never sent anywhere and is analyzed 100% locally.

Semgrep supports:

| **Python** | **Java** | **Go** | **JavaScript** | **C** | **Ruby** | **TypeScript** | **PHP**   |
| :--------- | :------- | :----- | :------------- | :---- | :------- | :------------- | :-------- |
| ✅         | ✅       | ✅     | ✅             | ✅    | 🚧       | Coming...      | Coming... |

Semgrep is proudly supported by [r2c](https://r2c.dev). For a fully-supported, hosted version of Semgrep please visit [r2c.dev]().

## Getting Started

Running a Semgrep rule is as easy as:

```sh
$ brew install semgrep && semgrep -e '$X==$X' /path/to/src
```

The Semgrep rule `$X==$X` will find all cases where the left and right hand side of an equality comparison are the same.

Semgrep patterns are very flexible. Learn how to write your own rules at [semgrep.live/learn]()

Want to scan your code with a community rule pack? Install Semgrep and scan code with the default rule pack by running:

```sh
$ semgrep --config=default
```

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
