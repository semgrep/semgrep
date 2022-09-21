This directory contains Github actions "workflows" to automate certain
tasks (e.g., running our testsuite on each PR, automate the release).
See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
for more information or our Notion page on Github actions.
Most of those workflows have the 'workflow_dispatch:' directive so you can
also trigger them manually here: https://github.com/returntocorp/semgrep/actions

Here is a short description of the workflows in this directory:

- lint.yml: running mostly pre-commit checks for our pull requests (PRs)
- tests.yml:
- semgrep.yml:
- start-release.yml:
- release.yml:
- e2e-semgrep-ci.yml:
- find-old-brew-prs.yml:
- homebrew-core-head.yml:
- open-bump-pr.yml:
- revert-semgrep-docker-image.yml:
- update-semgrep-rules.yml:
- validate-release.yml:
