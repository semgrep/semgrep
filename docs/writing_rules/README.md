# Writing Semgrep Rules

This page describes the thought process behind writing a new Semgrep rule and
gives a methodology for doing so effectively.

These docs focus on the process and solving example problems. For the
authoritative, detailed docs on:
* The power of Semgrep patterns, [see here](../pattern-features.md).
* YAML configuration options like `pattern-either`, `pattern-inside`, and more,
  [see here](../configuration-files.md).

> **For walkthrough examples of writing rules, see [these docs](examples.md), or an interactive, example-based tutorial here: https://semgrep.dev/learn.**

## A Rule Writing Methodology

There's no one right way or process for writing a Semgrep rule, do what works for you.

The following outlines a process we've found effective. At a high level, it's:
1. Brainstorm what you want to find
1. Determine how what you want to find concretely looks like in code
1. Create a sample source file with example code snippets you'd like to find,
   and write an initial Semgrep rule that matches it
1. Test your Semgrep rule and refine it by first running it on one real code
   repository, then across many
1. After you're happy with your rule's performance, integrate it into your
   continuous integration (CI) systems so that it's run on every pull request to
   ensure your code maintains a high quality bar.

### 1. Brainstorm What You Want to Find

First, determine what to look for.

It could be something very concrete and demand-driven, like:

#### a. Finding other instances of known vulnerable code patterns ("variant analysis"):
> Our pen test report / bug bounty program just reported an issue `<like this>`. I
> want to find all other similar code across all
> of my repos, because it might be vulnerable too.

#### b. Finding business logic bugs

> In my company, I know that our internal API method `foo()` must always be
> called a certain way. For example, one of the arguments *must* or *must not*
> be a hardcoded string, a certain flag or option should be set, etc. I want to
> find all the code locations where this implicit calling pattern is *not* being
> followed.

> In my applications, I know that one particular API call, `bar()` must *always*
> be called before another, `baz()`, or else it's a bug. I want to find all the
> places this calling convention isn't followed.

Or, your search could start a bit more abstract and exploratory, where you don't know exactly what you're looking for, like:

#### c. Auditing dangerous function use
> I know `eval()` is a potentially dangerous function, I want to see where it's
> used, and if so, how.

#### d. Auditing API or technology uses
> My company has a history of security issues with JWTs (or `<technology>`),
> let's find all of the code that touches JWT logic so we can audit it.

#### e. Reviewing authentication or authorization logic
> My company uses the `acme_corp_auth` library for all authentication or
> authorization logic. I want to review everywhere it's called to see if how the
> library is used makes sense, ensure that trust boundaries are enforced,
> confirm that enforcement is done consistently, and locate any places that are
> *missing* authn/authz.

These are a few examples, but in summary, the first step in writing a new rule
is determining:

> What is an interesting aspect of my code that I want to find?

### 2. Determine What That Concretely Looks like in Code

After you have a rough idea of what you want to find, the next step is to
identify what that concretely looks like in code. The more specific the better.

What does "*good*" or "*safe*" code look like?

For example, after reading some internal code and reading API docs, you find that for the Python `subprocess` module, `call()` and other methods are more dangerous when passed the argument `shell=True`, so you decide:

> I want to find all calls to `subprocess.call()` in which one of the keyword arguments is `shell=True`.

Or perhaps you're auditing Java Spring applications and you want to find all routes that do not perform authorization checks. You review some example routes in your internal repos and see code like this:

```java
@Controller
@RequestMapping("/api/")
public class AcmeController {

    @RequestMapping(method = RequestMethod.POST)
    @Authorize(Permissions.ADMIN)
    @ResponseBody
    public ResponseEntity<Map<String, Object>> createProfile() {
        return new ResponseEntity<>(result, HttpStatus.OK);
    }

    @RequestMapping(method = RequestMethod.GET)
    @ResponseBody
    public ResponseEntity<Map<String, Object>> showResults() {
        return new ResponseEntity<>(result, HttpStatus.OK);
    }
}
```

So you determine:

> I want to find all routes, which are methods with the the `@RequestMapping` annotation, that **do not** also have an `@Authorize` annotation.

The key at this stage is to go from an *idea* to one or more *concrete example code snippets* that demonstrate secure and/or insecure examples of what you're looking for.

### 3. Create a Test File, Write an Initial Rule

Now the fun part begins!

Create a YAML file in your current working directory, for example, the project root of a repo you intend to scan. You can name the YAML file whatever you want, but Semgrep will look for rules defined in a file named `.semgrep.yml` by default.

```yaml
rules:
  - id: my-pattern-name
    pattern: |
      TODO
    message: "Some message to display to the user"
    languages: [python]
    severity: ERROR
```

Create an example test file, containing snippets of code that **should** and **should not** match.

Fill out the `TODO` in the above YAML file with a Semgrep pattern, using various [pattern features](../pattern-features.md) and [additional pattern clauses](../configuration-files.md) as needed.

For additional help, see [these docs](examples.md) for walkthrough rule writing examples, or an interactive, example-based tutorial here: https://semgrep.dev/learn.

Check that the rule you're writing matches the example file by running:

```bash
$ semgrep example_file.py
$ semgrep --config my-rule-file.yml
```

#### Break it down

If you're writing a complex, multi-part pattern, rather than writing the whole pattern and then testing it, similar to writing a complex chunk of code, try building it in pieces, and testing each step along the way.

**`pattern` and `pattern-not`**: If there are a number of cases for the code you're searching for that you want to filter out, first write an initial pattern (using `pattern`) that finds the general case, and ensure it works as you expect.

Then, add a series of `pattern-not` clauses to filter out the cases that you *don't* want to match, that would cause your Semgrep rule to return "false positives."

**`pattern-either`**: If there are a series of code snippets you'd like to find that can't all be matched by the same pattern, create a test file with each of these code snippets, and then write a `pattern` clause for each.

Then, combine all of these clauses under a `pattern-either` clause, which will cause Semgrep to match code for which any subclause matches. Your Semgrep rule will look something like this:

```yaml
rules:
  - id: my-pattern-name
    patterns:
      pattern-either:
      - pattern: |
          <first pattern here>
      - pattern: |
          <second pattern here>
      - pattern: |
          <and however many more you want to match...>
    message: "Some message to display to the user"
    languages: [java]
    severity: ERROR
```

See [here](https://semgrep.dev/YG3X) for an example of using `pattern-either`.

### 4. Iterate and Refine

After you have a Semgrep rule that's been vetted against some test examples you created, it's time to see how it performs on real code.

### 4a. Test on One Repo

Clone one repo locally, if you don't have one ready, and scan it:

```yaml
$ semgrep --config path/to/my-rules.yml path/to/repo
```

Look through the results. Are you finding what you intended to find?

#### False Positives

*False positives*, in simplest terms, are results that your tool gives you that *are not* code instances you care about.

Review Semgrep's output: for the code locations that your pattern matched, are they "interesting" code snippets that you intended to match?

If not, what makes them *not* interesting?
* If there are multiple results that you'd like to filter out in the future, is there a common reason that makes them something you'd like to filter?
* e.g. "Whenever `foo()` is called before `check_auth()`, the result is generally something I don't care about, because of *{business logic reasons}*." or "If the second argument is a hardcoded string, the method call is safe."

#### False Negatives

*False negatives* are source code locations that you intended to find but that your pattern *missed*

In the general case, regardless of the tool you use, it's difficult to impossible to eliminate all false negatives. Provably finding every bug *can* be done, in some specialized systems, with a massive amount of work, using techniques like [formal methods](https://en.wikipedia.org/wiki/Formal_methods).

But in most contexts, in terms of "what can I practically do right now without person-years of effort," you can use the trusty [ripgrep](https://github.com/BurntSushi/ripgrep) to find code your Semgrep rule may have missed.

If your pattern involves a specific method call or annotation, search the code base for all references to the string and manually audit them to determine if they are code locations your Semgrep rule should have matched.

```bash
# -C returns a file lines of code above
# and below the match
$ rg -C 5 "exec\(" .

$ rg -C 5 "@RequestMapping" .
```

### 4b. Test on Multiple Repos

In the previous step, you tested your Semgrep rule on one real code base and iterated on the rule to ensure it catches the code patterns you intended (decreasing *false negatives*) and limiting the cases where it matches code you don't want to match (*false positives*).

Now it's time to vet your rule on a bigger corpus.

Gather a number of additional repos to test on that contain the programming language and functionality that are relevant to the rule you've been writing.

Note that if you put the target repos into the same directory, you can scan all of them at once by running Semgrep in the parent directory:

```yaml
# Scans all repos inside the current working directory
$ semgrep --config path/to/my-rules.yml .
```

As before, review the results, and iterate on the rule to make it more precise.

### 5. Roll Out to CI

Finding individual instances of bugs or antipatterns is nice, but ultimately,
what's more impactful to *continuously* scanning your code for issues, and
either *alerting on* or *blocking* bad code as soon as it's entered.

Staying on top of this manually would be a lot of work, so it's easier to have
this done automatically by including Semgrep in your continuous integration (CI)
so that your existing infrastructure handles scanning for you.

See the [integration docs](../integrations.md) for details on how to integrate
Semgrep into platforms including AppVeyor, CircleCI, TravisCI, GitHub Actions,
and GitLab.

You may also want to spend some time considering how you plan to handle Semgrep results.
* **Blocking the build**: In some cases, you may want to fail the build if
  certain rules trigger, as you have high confidence that they've identified an
  impactful security issue that needs to be fixed.
* **Alerting**: In other cases, Semgrep may identify code patterns that are
  security-relevant, but are not necessarily vulnerabilities. For these, it
  likely doesn't make sense to fail the build.
  * Instead, the security team can be notified, for example, in an `#appsec`
    Slack channel, that they may want to do a code review or reach out to the
    corresponding developer for more context.

#### 5a. Make data-driven improvements to rules based on user feedback

Despite your best efforts, there will likely be edge cases that occur in practice that trip up the rule you've worked so hard writing - either vulnerable code that it misses, or safe code that it incorrectly identifies.

The key is to build and maintain close relationships with engineering teams so that they feel comfortable giving you feedback when your rules aren't performing as well as you'd like, so that the rules can be improved.

If possible, collect metrics around your continuous code scanning, like:
* For each rule, how often does it fire? (in total, and per repo)
* For each rule, when it fires, how often do developers perceive it to be a real issue vs a false positive?
  * How often do developers fix the underlying code?

Making it easy for developers to provide feedback on the signal quality of a
rule is quite valuable for building a continuous scanning system that both
provides real security value and is appreciated by engineers. See [Tricorder:
Building a Program Analysis Ecosystem](https://research.google/pubs/pub43322/)
whitepaper by Google for more details on creating analysis feedback loops.

