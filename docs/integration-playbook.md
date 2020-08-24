# Semgrep Integration Playbook

This document describes common integration methods for you, the intrepid
integrator, to include Semgrep in your organization's workflows. This includes,
but is not limited to, strategies for when and where to include Semgrep, advice
on managing Semgrep in large codebases with many pre-existing issues, using
Semgrep to help ensure the security of your organization, etc.

Contents:

* [Integration Points](#integration-points)
  * [Development](#development)
  * [Security](#security)
* [Lifecycle Management](#lifecycle-management)
  * [Installing Semgrep](#installing-semgrep)
  * [Managing Rules](#managing-rules)
  * [Performance Considerations](#performance-considerations)

## Integration Points

This section describes common integration points where Semgrep is effective.

### Development

Integrating Semgrep into development workflows allows you to catch bugs and
security issues before they reach production environments. There are many
points where Semgrep can be integrated into development workflows. Generally
speaking, integrating Semgrep earlier in the development pipeline allows you
to catch mistakes sooner and easily fix any issues it surfaces. Correcting an
issue in your code editor is much easier than waiting days or weeks for a
security or QA team to surface the issue. However, CI systems are often more
reliable and repeatable than results from developer's environments. This
section will move from earliest to latest in the development pipeline.

**One-off Analyses**

Of course, Semgrep can always be [run locally](https://github.com/returntocorp/semgrep#getting-started)
to investigate findings, search for new patterns, or simply to tinker with the
tool. Running one-off analyses provides maximal flexbility while losing
reliability, repeatablility, and consistency. We recommend using this approach
during initial investigations and then moving to one of the below approaches
once the tool has been tuned and [configured](https://github.com/returntocorp/semgrep/blob/develop/docs/configuration-files.md)
to your environemnt.

**In-editor**

In-editor integration allows you to see Semgrep findings inside your editor.
This is the fastest way to get Semgrep results and offers the convenience of
seeing findings alongside the code you're editing.

In-editor integration is available for the following code editors:

* [Visual Studio Code](https://marketplace.visualstudio.com/items?itemName=semgrep.semgrep)

**Git Hooks**

Git hooks integration allows you to check for findings before code is pushed
to a remote repository. This allows you to quickly see issues without waiting
for a CI process to run or a code reviewer to notice them.

r2c recommends using the [`pre-commit`](https://pre-commit.com/) framework for
Git hook integration. However, the following is a complete listing of Git hook
integrations:

* [`pre-commit`](https://github.com/returntocorp/semgrep/blob/develop/docs/integrations.md#pre-commit-hook)
* [Git Hooks](https://www.atlassian.com/git/tutorials/git-hooks)

**Continuous Integration**

Continuous Integration (CI) allows you to check each commit for findings and
halt the run if necessary. CI is typically invoked on a remote server when
changes are pushed to a [VCS](https://en.wikipedia.org/wiki/Version_control).
Including Semgrep in CI gives the most assurance that no findings will fall
through the cracks. CI environments are often more reliable than expecting
uniformity and consistency across many development environments.

The following is a complete listing of CI integration avenues alongside our
existing CI provider documentation:

* [Semgrep CI Providers](https://github.com/returntocorp/semgrep/blob/develop/docs/integrations.md#continuous-integration)
* [`semgrep-action` GitHub Action](https://github.com/marketplace/actions/semgrep-action)
* [Coinbase Salus](https://github.com/coinbase/salus)

### Security

Historically, in many companies, there was periodic friction between
engineering and security teams when developers wanted to push forward and
release new features quickly, while the security teams wanted to slow down or
stop new releases until it could be ensured that the new code met a certain
security bar.

But times have changed. Now, it's essential for modern security teams to enable
the engineering teams they support to move quickly and securely, by choosing
the right solutions, technologies, tools, and products that reduce friction as
much as possible. This is what Semgreps aims to help security teams achieve:
being business enablers with a seat at the table.

Security teams must be available for questions and provide guidance, while not
letting perfect be the enemy of good. We must enable confidence and take on the
bulk of integration work since we are fundamentally the ones making a request.

This section will take a different approach than the development section above.
Security will often be making requests of development teams and thus will likely
need to reference the above integration points. However, this section will
instead discuss pre-integration research, integration point trade-offs,
smoothly rolling out Semgrep, etc. In other words, strategic considerations
instead of purely technical capabilities.

**Pre-integration Research**

Before integrating Semgrep into your workflows you will likely need to do a
bit of preliminary research. This includes asking yourself questions like:
What languages, frameworks, and technologies is this project using? What
vulnerability classes am I concerned about? What integration point(s) can I
leverage to maximize developer productivity while minimizing risk of security
issues? Research will likely start with a **one-off analysis** as mentioned
above. This section will discuss trade-offs to consider while performing
pre-integration research.

After performing a language and framework asset inventory you may start to
build a corpus of rules you'd like to run against the project. r2c has developed
many openly available rule sets. E.g.

* You discover mostly Python code: [`/p/python`](https://semgrep.dev/p/python)
* You're concerned about Golang security: [`/p/gosec`](https://semgrep.dev/p/gosec)
* You'd like to replace your slow FindSecBugs tooling: [`/p/findsecbugs`](https://semgrep.dev/p/findsecbugs)
* You discover Flask web server usage: [`/p/flask`](https://semgrep.dev/p/flask)

Although security is the primary focus, there are also many packs that look
for non-security related issues. Consider your primary concerns when thinking
about rule selection. If you don't find what you're looking for, Semgrep makes
it very easy to create your own rules [locally](https://github.com/returntocorp/semgrep/blob/develop/docs/configuration-files.md)
or via r2c's [live editor](https://semgrep.dev/editor). Familiarizing yourself
with Semgrep rule writing is a great way to create rules for project-specific
patterns for your team or organization.

After you have your rules selected it's time to run them against the project.
This will often reveal noisy rules, unhelpful rules, or false positives. To
maintain forward momentum it's often better to disable problematic rules and
revisit them in the future instead of trying to fix everything at once. The
[Managing Rules](#managing-rules) section below will provide more details on
this.

**Integration Point Trade-offs**

Generally speaking, the earlier you include Semgrep in the development workflow
the faster you'll get results and quicker you'll be able to correct the
violation. However, earlier also means you'll have less reliable results and
less strict guarantees on what's entering the codebase. For example, consider
some of the following trade-offs:

* One-off analyses vs. in-editor integration
  * One-off analyses are typically quicker to set up and faster to run,
    but aren't consistently applied. They're great for security research
    tasks and point-in-time analysis, but don't provide continuous analysis.
* In-editor integration vs. Git hooks integration 
  * In-editor integration provides the fastest feedback, but may not be suitable
    for comprehensive analysis. Large rule sets will naturally take more time,
    which may interupt the development workflow.
  * Editor choice, and thus in-editor integrations, is typically up to the
    individual developer.
  * Security teams may not be able to influence applications used on
    development machines.
* Git hooks integration vs. CI integration
  * Git hooks allow for faster, local feedback vs. a remote CI system.
  * Git hooks can be disabled locally, so the analysis is not guaranteed
    to happen.
  * CI is often a consistent, clean environment from which to run your analysis.
  * Security teams may not be able to influence what runs in CI. This means
    one-off analyses may be necessary. On the other hand, security teams may not
    have direct code access, and may have to use CI for this purpose.
  * Git hooks or in-editor integrations will naturally have different
    performance requirements vs. CI and/or nightly analysis jobs. Developers
    cannot wait minutes or hours for their editor command or commit to finish,
    but a comprehensive nightly job may be permitted this runtime. This
    trade-off will be discussed in more detail below in
    [Performance Considerations](#performance-considerations).

There's no single solution for all teams or environments. Further, the above
list is not an exhaustive list of trade-offs you may have to make. This is
simply a starting point to get you thinking. Remember that more is not
necessarily better here. Trying to plug-in to all available integration points
and enabling as many rules as possible may be detrimental to the long-term
success of your project. Start slow and small, build confidence, slowly expand
scope, rinse and repeat.

**Semgrep Rollout**

TODO

## Lifecycle Management

TODO

### Installing Semgrep

TODO

### Managing Rules

TODO

### Performance Considerations

TODO
