This directory contains Github actions (GHA) "workflows" to automate certain
tasks (e.g., running our testsuite on each PR, automate the release).
Basically help to build, test, and deploy Semgrep.
See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
for more information on GHA or our Notion page on "Github actions".

Most of those workflows have the 'workflow_dispatch:' directive so you can
also trigger them manually here: https://github.com/returntocorp/semgrep/actions

Here is a short description of the workflows in this directory:

- lint.yml: running mostly pre-commit checks for our pull requests (PRs).
  It runs the same checks that we run with pre-commit locally (configured
  in semgrep/.pre-commit-config.yaml), but enforced here in CI in case
  the developer forgot to setup pre-commit.

- tests.yml: building Semgrep and running our Semgrep testsuite for our PRs.
  It also runs benchmarks, test our MacOS, Linux, and Docker build, and more.

- semgrep.yml: dogfood Semgrep by using our Semgrep Github action
  and submitting findings to Semgrep App for bugs in the semgrep repo itself.

- start-release.yml: workflow to manually trigger a new Semgrep release.
  This internally creates a new branch, calls 'make release' on it, and
  puts it on a vXxx branch which then triggers the release workflow below.
  It also coordinates separate workflows, waits for them to finish to
  start other workflows. Here are the dependent workflows:

  - release.yml: this workflow is triggered after start-release.yml created
    a candidate release PR and pushed a vXxx branch. It then performs lots of tasks
    such as creating the Linux and MacOS binary artifacts,
    publishing to PyPy, HomeBrew, updating our Semgrep develop Docker image, etc.

  - validate-release.yml: this workflow is activated by start-release.yml
    once release.yml has finished. It performs checks to make sure
    the artifacts produced by release.yml are valid.

  - open-bump-pr.yml: this workflow is activated once we have a new Semgrep release
    (after validate-release.yml) and in turn bumps to the new Semgrep version
    many dependent repositories (semgrep-rpc, semgrep-action, semgrep-app)

- homebrew-core-head.yml: cron to check that the new Homebrew Core Formula
  created by release.yml works. Why can't this be part of validate-release.yml?
  Because it usually takes some time for HomeBrew developers to accept the PR
  created in release.yml, so we shouldn't block on actions we can't control.
  This is why we have the find-old-brew-prs.yml and homebrew-core-head.yml
  workflows.

- find-old-brew-prs.yml: cron to check whether the HomeBrew PR created by
  release.yml has been accepted or is hanging around unmerged.

- update-semgrep-rules.yml: cron to update semgrep/semgrep-core/tests/semgrep-rules
  submodule to its latest version

- revert-semgrep-docker-image.yml: interactive workflow
  to manually revert the Semgrep 'latest' Docker image.

- e2e-semgrep-ci.yml: end-to-end testing of the 'semgrep ci' subcommand,
  and our 'develop' Docker image, which are used by all the users of Semgrep App.
