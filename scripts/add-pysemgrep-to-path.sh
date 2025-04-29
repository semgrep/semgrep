#!/usr/bin/env sh
# setup pipenv for people automatically
export PIPENV_PIPFILE='./cli/Pipfile'
if ! pipenv --venv >/dev/null; then
    echo "Pipenv not setup yet, doing so now"
    pipenv install
fi
PYSEMGREP_PATH="$(pipenv --venv)/bin"
# shellcheck disable=SC3010
if [[ $PATH != *"$PYSEMGREP_PATH"* ]]; then
    echo "adding pysemgrep's bin to path, you can now call 'semgrep' directly"
    export PATH="$PYSEMGREP_PATH:$PATH"
fi
unset PIPENV_PIPFILE
