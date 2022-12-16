regexp parsers
==============

The same code base provides a configurable parser that can support the
most popular regexp dialects similar to Perl's syntax. These
include the syntaxes supported by the following frameworks:

* Perl
* PCRE (Perl-compatible regular expressions), a C library.
* Python regex
* JavaScript
* (many more)

Those are all syntaxes where special characters are first-class
e.g. `a*` means "repeat character `a` 0 or more times" and `a\*` is a
sequence of an `a` followed by a literal `*`.

Unsupported syntaxes include:

* `sed -e`
* grep's basic regexps (`grep`, `grep -G`), extended regexps (`grep -E`)
* emacs-style regexps
* OCaml's `str` library
