## Pre-commit

### The pre-commit test is failing on Github

Make sure to follow the [Development Workflow](#development-workflow) so that pre-commit will run on commit

### I can't commit because code I haven't touched is failing pre-commit

Sometimes changes you make will cause pre-commit errors in code you haven't touched--for example, if you change a function's return type. However, if you're absolutely sure you didn't cause this, you can run `git commit --no-verify` to commit without running `pre-commit`.
