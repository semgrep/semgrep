# Parsing statistics

_This folder is independent from the rest of the semgrep project. It only
has `semgrep-core` as a dependency, and the CI setup in `.circleci`
and/or `.github`. We could move it to another repo if desired._

To test the parsing stats:

```bash
$ make test
```

To run the stats on all the languages in parallel:

```bash
$ make  # or ./run-all
```

To run the stats for just one language:

```bash
$ ./run-lang java  # or whatever language you want
```
