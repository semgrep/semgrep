# Contributing to pfff

Pfff welcomes contributions from anyone. To contribute:

1. Please make sure you read and agree to the
[Contributor License Agreement](https://cla-assistant.io/returntocorp/pfff).
The purpose of the CLA is to allow future relicensing without having to
track down any past contributor. By signing the CLA, **your contribution will
remain under the current license (LGPL) regardless of future changes**.
2. If you have an idea for a feature or notice a bug please
[open an issue](https://github.com/returntocorp/pfff/issues/new/choose).
Creating an issue first is preferable to moving directly to a pull request so
that we can ensure you're on the right track without any wasted effort. This
is also a great way to contribute to pfff even if you're not making changes
yourself.

## Developer setup

Install opam if you don't have it already. Then install
pfff's dependencies using:

```
./scripts/install-opam-deps
```

Before your first commit, you'll need to set up a pre-commit hook
which will normalize the formatting of source files. For this, you
need to install the `pre-commit` command.

Install python3 and pip3 for your environment:

```
$ pip3 install pre-commit
```

Then activate the pre-commit hook for this repository:

```
$ pre-commit install
```

Now you can run `git commit`. Note that this uses the
`ocp-indent` executable, which should have been installed with the
other opam packages earlier.
