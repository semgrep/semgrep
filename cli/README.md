# semgrep-cli Development

Semgrep uses pipenv to manage a virtual environment:

```
pipenv install --dev
pipenv shell
pytest
semgrep --help
```

**NOTE:** When installed via pipenv, semgrep-core is not installed. _Bring your own semgrep core_.

## Dependencies

* Dev dependencies like pytest and tox should be installed via pipenv. 
* Runtime dependencies _may not be installed via pipenv_. They must be specified in `setup.py`.
