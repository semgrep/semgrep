<!--
   Unfortunately GitHub doesn't render symlinks as clickable, otherwise
   this file would be a symlink.
-->

Thank you for your interest in contributing to the Semgrep source code!

Find contribution guidelines in **[Semgrep documentation](https://semgrep.dev/docs/contributing/contributing/)**.

Specifically, see **[Contributing code](https://semgrep.dev/docs/contributing/contributing-code/)** to contribute either to Semgrep source code or semgrep-core source code.

## Quick start
Here's a quick way to get Semgrep building from source. See the documentation referenced above for detailed instructions

### Install prerequisites
The following dependencies need to be installed:
* [uv](https://docs.astral.sh/uv/getting-started/installation/)
* [bash](https://www.gnu.org/software/bash/)
* [opam](https://opam.ocaml.org/)

To install these on MacOS, run `make configure-osx`. Otherwise install these dependencies through the appropriate package manager, and then run `make configure`.

Now we can setup the OCaml dependencies, and install Semgrep:

```sh
# Install OCaml dependencies
make setup
# Build the OCaml binaries under `bin/`
make
# install the CLI wrapper in `cli/` in editable mode
# w/freshly built OCaml binary included
make install
# run tests
make test
```
