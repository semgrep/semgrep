semgrep-core tests
==
This folder contains mostly tests related to the semgrep-core program.
For the (py)semgrep tests see the cli/tests/ folder instead.

Running the tests
--

All the semgrep-core tests can be run from the project's root
with `make core-test`. This will recompile the test programs if necessary.

Only running the main test program can be done with `./test`. Try
`./test --help` for options. In particular, running just the tests
that contain the string `php` is done with
```
$ ./test -s 'php'
```

⚠️ `./test` won't try to rebuild the test program to make things
faster. It is recommended to run `./test` when editing language files
(`.sgrep` files and targets) because they will be picked up
automatically without having to rebuild the test program.

Matching tests
--

The `patterns/` folder contains pairs (pattern, target). A pattern must be a
file with the `.sgrep` extension and a target must be a file in the target
language with the language's usual extension, known to semgrep-core.
For example, the folder for the Go language looks like this:

```
go
├── ...
...
├── shortassign.go
└── shortassign.sgrep
```

Some simple patterns are identical from one language to
another. As a convenience, if the `.sgrep` file exists in the
`POLYGLOT` folder, there's no need to have it in the language's
folder. For instance, we have:

```
go
├── ...
└── regexp_string.go
POLYGLOT
├── ...
└── regexp_string.sgrep
```

These test cases are picked up by the test program. If a match is
expected, the line just before the match must contain the string `ERROR:` or
the string `MATCH:`. As an example, the following target file expects
exactly two matches `eval(s)` and `eval(read_line())`:

```
# Don't match if the argument is a literal string.
eval("echo hello")

# MATCH:
eval(s)

# MATCH: on external input
eval(read_line())
```

Parsing tests
--

Tests for just parsing are specified by placing source files into the
language's `parsing/` as follows:

```
parsing
├── ...
├── go
│   ├── if_header_type.go
│   ├── string.go
│   └── unicode_rune.go
...
└── ...
```

The expectation is that each file will be parsed without an error.

JSON/Web cheatsheet
--

The semgrep.dev live editor displays a cheatsheet for the selected
language. This cheatsheet reuses some of the examples found in the
language's folder. For details on what goes into the cheatsheet and
how to generate it, look into the scripts:

- `/scripts/generate-cheatsheet`
- `/scripts/generate_cheatsheet.py`
