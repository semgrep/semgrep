# Aliengrep

Aliengrep is a generic mode design to complement and address the
shortcomings of Spacegrep aka Semgrep's generic mode.

Aliengrep is designed as an easy way to write patterns for matching
text targets. It is meant to be easier and more Semgrep-friendly than
regexps and their usual syntax.

## Example

The pattern

```
hello($...ARGS)
```

matches

```
hello(first_name, last_name)
```

but also

```
hello ()
```

and

```
hello(
  get_name()
)
```

and

```
hello(get_name())
```

## Features

The major differences with Spacegrep are:

- Indentation in patterns or target code doesn't matter. All
  whitespace is treated as optional.
- The sets of characters that make up words and braces are customizable.
- There's no limit on how many lines an ellipsis can match (`...` in
  multiline mode or `....` in single-line mode).
- In single-line mode, newlines in patterns are matched literally and
  regular ellipses (three dots) will not match newlines.
- [experimental] In single-line mode, a long ellipsis `....` (four dots) or
  `$....FOO` allows matching newlines as ordinary whitespace.

Other features:

- Like in Semgrep and Spacegrep, patterns support ellipses (`...`) and
  metavariables (`$FOO`, `$...FOO`).
- Like in Spacegrep, a regular metavariable such as `$FOO` captures a
  single word.
- Like in Spacegrep, whitespace between non-word characters, words, or
  braces is ignored and serves only as a separator between lexical
  elements. Matching special character sequences such as `$FOO` or
  `...` literally is done by inserting spaces e.g. `$ FOO` or `. . .`.
- Unlike in Spacegrep, patterns and target code must be
  UTF-8-compatible.
- Braces are matched when possible e.g. the pattern `(...)` matches
  `((x), y)`, not just `((x)`.

## Implementation

To allow for custom word characters and custom braces, the
implementation doesn't use ocamllex. Patterns are parsed with a custom
parser and translated into a PCRE pattern. Target code is not parsed
per se but directly used to scan target code.

## Semgrep integration

In a Semgrep rule, aliengrep is a specific engine for analyzing
generic targets. It's specified as an option in the `options`
section.
