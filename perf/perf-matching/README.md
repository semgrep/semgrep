# Matching Benchmarks

This is the place for benchmarks focused on matching performance. Welcome!

A collection of tests is defined. We regularly run each of these tests and
track their duration. The results are uploaded to the semgrep dashboard at
https://dashboard.semgrep.dev/

The list of tests is specified in the script 'run-perf-suite'.
The input files used in the tests are in the 'input' folder. For test 'foo',
there must be one file 'input/foo.sgrep' that contains the semgrep pattern.
The target file can be any file in 'input/'.
