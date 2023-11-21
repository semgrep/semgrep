# Build instructions for developers

## Manual development

Developers should consult the makefiles, which are documented.
The steps to set up and build everything are normally:

```
$ git submodule update --init --recursive
$ make setup       # meant to be run infrequently, may not be sufficient
$ make             # routine build
$ make test        # test everything
```

There's no simple installation of the development version of the
`semgrep` command (Python wrapper + `semgrep-core` binary). To test
`semgrep` without installing it, use `pipenv`:

```
$ cd semgrep
$ pipenv shell
$ semgrep --help
```

Or more conveniently, you can create a shell function that will call
`pipenv` from the correct location. For example, if you cloned the
`semgrep` repo in your home folder (`~`), you can place the following
code in your `~/.bashrc` file and then use `semgrep-dev` as your
`semgrep` command:

```
semgrep-dev() {
  PIPENV_PIPFILE=~/semgrep/cli/Pipfile pipenv run semgrep "$@"
}
```

The Semgrep project has two main parts:

- The Python wrapper in the [`cli/`](cli) folder, which has its own
  makefile needed for some preprocessing and for testing.
  Read the makefile to see what targets are available.
- The OCaml core in the [`src/`](semgrep-core) folder.
  Read the toplevel makefile to see what's available to the developer.

## Reproducible and standalone build with Docker

The main [`Dockerfile`](Dockerfile) serves as a reference on how to
build Semgrep for Linux. The usual instructions for building a Docker
image apply. It should be:

```
$ docker build -t semgrep .
```

## Contribution guidelines

Contribution guidelines and developer documentation
are [available from Semgrep's documentation
website](https://semgrep.dev/docs/contributing/contributing/).
