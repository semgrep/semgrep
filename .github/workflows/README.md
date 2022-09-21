This directory contains Github actions "workflows" to automate certain
tasks (e.g., running our testsuite on each PR, automate the release).
See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
for more information on Github actions or our Notion page on Github actions.

Most of those workflows have the 'workflow_dispatch:' directive so you can
also trigger them manually here: https://github.com/returntocorp/semgrep/actions

Here is a short description of the workflows in this directory:

- lint.yml: running mostly pre-commit checks for our pull requests (PRs).
  It runs the same checks that we run with pre-commit locally (configured
  in semgrep/.pre-commit-config.yaml), but enforced here in CI in case
  the developer forgot to setup pre-commit.

- tests.yml: building semgrep and running our semgrep testsuite for our PRs.
  It also runs benchmarks, test our MacOS, Linux, and docker build, and more.

- semgrep.yml: dogfood Semgrep by using our Semgrep Github action
  and submitting findings to Semgrep App.

- start-release.yml: workflow to manually trigger a new Semgrep release.
  This internally creates a new branch, call 'make release' on it, and
  put it on a vXxx branch which then triggers the release workflow below.
  It also coordinates separate workflows, wait for them to finish to
  start other workflows. Here are the dependent workflows:

  - release.yml: this workflow is triggered after start-release.yml created
    a candidate release PR and pushed a vXxx branch. It then performs lots of task such
    as creating the Linux and MacOS binary artifacts,
    publishing to PyPy, homebrew, updating our semgrep develop docker image, etc.

  - validate-release.yml: this workflow is activated by start-release.yml
    once release.yml finished. It performs checks to make sure
    the artifacts produced by release.yml are valid

  - open-bump-pr.yml: this workflow is activated once we have a new semgrep release
    (after validate-release.yml) and in turn bumps to the new semgrep version
    many dependent repositories (semgrep-rpc, semgrep-action, semgrep-app)

- find-old-brew-prs.yml: cron to check whether the homebrew PR created by
  release.yml has been accepted or is hanging around unmerged

- homebrew-core-head.yml: cron to check that the Homebrew Core Formula
  created by release.yml works (TODO: why can't this be part of validate-release.yml?
  because it usually take some time for homebrew developers to accept the PRs
  so we can't test directly? hence find-old-brew-prs.yml and homebrew-core-head.yml
  workflows?)

- update-semgrep-rules.yml: cron to update semgrep/semgrep-core/tests/semgrep-rules
  submodule to its latest version

- revert-semgrep-docker-image.yml: interactive workflow
  to manually revert the semgrep 'latest' docker image.

- e2e-semgrep-ci.yml: ???
