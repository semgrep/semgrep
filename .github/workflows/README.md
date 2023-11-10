This directory contains Github actions (GHA) "workflows" to automate certain
tasks (e.g., running our testsuite on each PR, automate the release).
Basically those workflows help to build, test, and deploy Semgrep.
See https://docs.github.com/en/actions/learn-github-actions/understanding-github-actions
for more information on GHA or our Notion page on "Github actions".

Most of those workflows have the 'workflow_dispatch:' directive so you can
also trigger them manually here: https://github.com/returntocorp/semgrep/actions

Note that many workflows are now written using Jsonnet
(see https://jsonnet.org/learning/tutorial.html for a great intro to Jsonnet)
instead of YAML. This allows to factorize lots of boilerplate by simply
defining Jsonnet functions or even simple constants. For the same
reason we've switched to Jsonnet for writing Semgrep rules instead of YAML,
we're switching to Jsonnet also for our GHA workflows.

!!DO NOT MODIFY THE .yml FILE THAT ARE GENERATED FROM a .jsonnet FILE!!
(the .yml file should contain a comment at the top warning against it).
Instead, modify the .jsonnet and simply run 'make' in this directory.

Here is a short description of the workflows in this directory:

- lint.jsonnet: running mostly pre-commit checks for our pull requests (PRs).
  It runs the same checks that we run with pre-commit locally (configured
  in semgrep/.pre-commit-config.yaml), but enforced here in CI in case
  the developer forgot to setup pre-commit.

- tests.jsonnet: building Semgrep and running our Semgrep testsuite for our PRs.
  It also runs benchmarks, test our MacOS, Linux, and Docker build, and more.

- semgrep.jsonnet: dogfood Semgrep by using our Semgrep Github action
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

- nightly.yml: cron to check that the Homebrew Core Formula works.
  Why can't this be part of validate-release.yml?
  Because it usually takes some time for HomeBrew developers to accept the PR
  created in release.yml, so we shouldn't block on actions we can't control.
  This is why we have the find-old-brew-prs.yml and homebrew-core-head.yml
  workflows.

- update-semgrep-rules.jsonnet: cron to update semgrep/tests/semgrep-rules
  submodule to its latest version

- revert-semgrep-docker-image.yml: interactive workflow
  to manually revert the Semgrep 'latest' Docker image.

- e2e-semgrep-ci.yml: end-to-end testing of the 'semgrep ci' subcommand,
  and our 'develop' Docker image, which are used by all the users of Semgrep App.
