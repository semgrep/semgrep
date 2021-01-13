toy matcher
==

This folder contains an implementation of toy pattern matcher that
operates on a sequence of symbols, rather than a tree. The
goal is to document, evaluate, and test matching
algorithms used in semgrep-core and spacegrep.

Usage
--

This software only provides source code and a test suite.
* To check for correctness, run `make test`.
* To show durations and other details, run `make bench`.

Experiment with new tweaks by editing the source code. This is meant
as an easy and consequence-free alternative to editing semgrep-core
directly.
