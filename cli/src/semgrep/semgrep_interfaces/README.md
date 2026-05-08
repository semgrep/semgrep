# semgrep-interfaces

This repository contains IDL (Interface Definition Language) files specifying
the interface between different Semgrep components (e.g., between
the semgrep CLI and the playground). It also includes the schema for Semgrep rules,
as both Semgrep and Semgrep App rely on this.

This repository is meant to be used as a submodule.

You may need to install opam and mypy as pre-requisites for contributing to this repository.

To get setup in this repositoty:
1. install [uv](https://docs.astral.sh/uv/getting-started/installation/)
2. Run `make dev-setup`

To update an interface:
1. Make changes to the appropriate .atd file or edit `generate.py`
2. Run `make`. This will propagate that change to the respective .py, .ts, .ml, etc.
3. Run `make test`. This will check backwards compatibility, validate schemas, and typecheck python files

---

The code in this repository is licensed under the terms of the [LGPL
2.1](LICENSE). For more information about the licensing details of Semgrep
itself see [our licensing page][semgrep-licensing].

[semgrep-licensing]: https://semgrep.dev/docs/licensing/
