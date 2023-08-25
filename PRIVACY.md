# Semgrep Privacy Policy

Semgrep may collect aggregate metrics to help improve the product. This document describes:

- [the principles that guide our data-collection decisions](#principles)
- [how to change when Semgrep sends metrics](#automatic-collection-opt-in-and-opt-out)
- [what data is not collected](#data-not-collected)
- [what data is collected](#data-collected)

## Principles

These principles inform our decisions around data collection:

1. **Transparency**: Collect and use data in a way that is clearly explained to the user and benefits them
2. **User control**: Put users in control of their data at all times
3. **Limited data**: Collect what is needed, pseudoanonymize where possible, and delete when no longer necessary

## Automatic collection, opt-in, and opt-out

```sh
$ semgrep --config=myrule.yaml  # → no metrics (loading rules from local file)
$ semgrep --config=p/python     # → metrics enabled (fetching Registry)
$ semgrep login && semgrep ci   # → metrics enabled (logged in to semgrep.dev)
```

Semgrep does **not** enable metrics when running with only local configuration files or command-line search patterns.

Semgrep does enable metrics if rules are loaded from the [Semgrep Registry](https://semgrep.dev/r).
This helps maintainers improve the correctness and performance of registry rules.

Metrics may also be configured to be sent on every run, or never sent.

To configure metrics, pass the `--metrics` option to Semgrep:

- `--metrics auto`: (default) metrics are sent whenever rules are pulled from the [Semgrep Registry](https://semgrep.dev/r)
- `--metrics on`: metrics are sent on every Semgrep run
- `--metrics off`: metrics are never sent

Alternatively, set the `SEMGREP_SEND_METRICS` environment variable to `auto`, `on`, or `off`.

Note that certain Semgrep integrators turn on metrics for every run.
For example, [GitLab's Semgrep SAST analyzer](https://gitlab.com/gitlab-org/security-products/analyzers/semgrep) uses `--metrics on` by default.

## Data NOT collected

### Data NOT collected ever

We strive to balance our desire to collect data for improving Semgrep
with our users' need for privacy and security.
After all, we are a security tool!
The following never leave your environment and are not sent or shared with anyone.

- Source code
- Private rules

### Data NOT collected unless explicitly requested

The following data will never leave your environment as part of metrics.

- Filenames
- Git commit hashes, timestamps, messages, authors
- User-identifiable data about Semgrep’s findings in your code, including finding messages

This data will be sent to Semgrep App only if you explicitly request it,
such as with `semgrep login && semgrep ci` to connect with Semgrep App.
Even in that case, your source code and private rules will never be sent.

## Data collected as metrics

Semgrep collects data to improve the user experience.
Five types of data are collected:

### Environmental

Environmental data provide contextual data about Semgrep’s runtime environment, as well as information that helps debug any issues users may be facing; e.g.

- How long the command took to run
- The version of Semgrep
- An [anonymous user ID](#anonymous-user-id) that identifies the machine
- IP address that triggers a run
- Value of the CI environment variable, if set
- Pseudoanonymized hash of the scanned project’s name
- Pseudoanonymized hash of the rule definitions run
- Pseduoanonymized hash of the config option.
  _(Note that when a config option downloads a ruleset from the https://semgrep.dev registry, [feature usage metrics](#feature-usage) still include the ruleset name in plain text.)_

### Performance

Performance data enable understanding of which rules and types of files are slow in the aggregate so Semgrep, Inc can improve the Semgrep program-analysis engine, query optimizer, and debug slow rules; e.g.

- Runtime duration
- Total number of rules
- Total number of files
- Project size in bytes

### Parse Rates

Semgrep reports aggregated parse rate information on a per-language basis; e.g.,

- Number of targeted files
- Number of files without any parse-related error
- Number of bytes across targeted files
- Number of bytes without any parse-related error

### Errors

High-level error and warning classes that Semgrep encounters when run; e.g.

- Semgrep’s return code
- The number of errors
- Compile-time error names, e.g., MaxFileSizeExceeded, SystemOutOfMemory, UnknownFileEncoding

### Value

Semgrep reports data that indicate how useful a run is for the end user; e.g.

- Number of raised findings
- Number of ignored findings
- Pseudoanonymized hashes of the rule definitions that yield findings
- The [Semgrep features used](#feature-usage) during the scan
- The engine type requested for the scan

### Pseudoanonymization

Certain identifying data (e.g. project URLs) are pseudoanonymized before being sent to the Semgrep, Inc backend.

"Pseudoanonymized" means the data are transformed using a deterministic cryptographically secure hash. When the input data are unknown, this hash is expensive to reverse. However, when input data are known, a reverse dictionary of identifiers to hashes can be built. Hence, data are anonymous only when the source values are unknown.

We use a deterministic hash to:

- Track performance and value improvements over successive runs on projects
- Remove test data from our metrics

Using a deterministic hash, however, implies:

- An entity that independently knows the value of an input datum AND who has access to Semgrep, Inc's metrics data could access metrics for that known datum

Semgrep, Inc will:

- Treat collected metrics data as secret, using application-security best practices, including (but not limited to)
  - Encryption during transit and rest
  - Strict access control to data-storage systems
  - Application-security-policy requirements for third parties (e.g. cloud-service providers; see "data sharing" below)
- Only correlate hashed data to input data when these inputs are already known to Semgrep, Inc (e.g. publicly available project URLs for open-source projects, or projects that log in to the Semgrep Registry)

## Description of metrics fields

| Category    | Field                                   | Description                                                                   | Use Case                                                                                   | Example Datum                                                                                                                                                                         | Type           |
| ----------- | --------------------------------------- | ----------------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------- |
| Environment |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
|             | Timestamps (started/sent)               | Time when the event fired                                                     | Understanding tool usage over time                                                         | 2021-05-10T21:05:06+00:00                                                                                                                                                             | String         |
|             | Event ID                                | A random UUID generated when sending the event.                               | Deduplicating events in case of issues during transmission                                 | 222bcccd-9dc2-4d10-ac3a-5692460e77ee                                                                                                                                                  | String         |
|             | [Anonymous User ID](#anonymous-user-id) | A random UUID generated on first run.                                         | Unique users per ruleset and feature. Understanding percentage of logged in users.         | 5f52484c-3f82-4779-9353-b29bbd3193b6                                                                                                                                                  | String         |
|             | Version                                 | Semgrep version being used                                                    | Reproduce and debug issues with specific versions                                          | 0.51.0                                                                                                                                                                                | String         |
|             | Project hash                            | One-way hash of the project URL                                               | Understand performance and accuracy improvements                                           | `c65437265631ab2566802d4d90797b27fbe0f608dceeb9451b979d1671c4bc1a`                                                                                                                    | String         |
|             | Rules hash                              | One-way hash of the rule definitions                                          | Understand performance improvements                                                        | `b03e452f389e5a86e56426c735afef13686b3e396499fc3c42561f36f6281c43`                                                                                                                    | String         |
|             | Config hash                             | One-way hash of the config argument                                           | Understand performance and accuracy improvements                                           | `ede96c41b57de3e857090fb3c486e69ad8efae3267bac4ac5fbc19dde7161094`                                                                                                                    | String         |
|             | Is authenticated                        | Whether the user logged in to semgrep.dev with `semgrep login`                | Understand popularity of logged in features                                                | `false`                                                                                                                                                                               | Boolean        |
|             | Integration name                        | If Semgrep is being called by another tool, optional name of that integration | Reproduce and debug issues specific integrations                                           | `gitlab`                                                                                                                                                                              | String         |
|             | CI                                      | Notes if Semgrep is running in CI and the name of the provider                | Reproduce and debug issues with specific CI providers                                      | GitLabCI v0.13.12                                                                                                                                                                     | String         |
|             | Client IP                               | IP address that triggered a run                                               | Understand broad ruleset usage                                                             | 0.0.0.0                                                                                                                                                                               | String         |
|             |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
| Performance |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
|             | Duration                                | How long the command took to run                                              | Understanding aggregate performance improvements and regressions                           | 14.13                                                                                                                                                                                 | Number         |
|             | Total Rules                             | Count of rules                                                                | Understand how duration is affected by #rules                                              | 137                                                                                                                                                                                   | Number         |
|             | Total Files                             | Count of files                                                                | Understand how duration is affected by #files                                              | 4378                                                                                                                                                                                  | Number         |
|             | Total Bytes                             | Summation of target file size                                                 | Understand how duration is related to total size of all target files                       | 40838239                                                                                                                                                                              | Number         |
|             | Rule Stats                              | Performance statistics (w/ rule hashes) for slowest rules                     | Debug rule performance                                                                     | `[{"ruleHash": "7c43c962dfdbc52882f80021e4d0ef2396e6a950867e81e5f61e68390ee9e166","parseTime": 0,"matchTime": 0.05480456352233887,"runTime": 0.20836973190307617,"bytesScanned": 0}]` | StatsClass[]   |
|             | File Stats                              | Performance statistics for slowest files                                      | Debug rule performance                                                                     | `[{"size": 6725,"numTimesScanned": 147,"parseTime": 0.013289928436279297,"matchTime": 0.05480456352233887,"runTime": 0.20836973190307617}]`                                           | StatsClass[]   |
|             |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
|             |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
| Parsing     |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
|             | Total Files                             | Count of files, on a per-language basis                                       | Understand parsing performance                                                             | 143                                                                                                                                                                                   | Number         |
|             | Total Bytes                             | Summation of target file size, likewise grouped                               | Understand parsing performance                                                             | 41244                                                                                                                                                                                 | Number         |
|             | Parsed Files                            | Count of files without parse errors                                           | Understand parsing performance                                                             | 140                                                                                                                                                                                   | Number         |
|             | Parsed Bytes                            | Count of bytes without any parse errors                                       | Understand parsing performance                                                             | 40312                                                                                                                                                                                 | Number         |
|             |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
| Errors      |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
|             | Exit Code                               | Numeric exit code                                                             | Debug commonly occurring issues and aggregate error counts                                 | 1                                                                                                                                                                                     | Number         |
|             | Number of Errors                        | Count of errors                                                               | Understanding avg #errors                                                                  | 2                                                                                                                                                                                     | Number         |
|             | Number of Warnings                      | Count of warnings                                                             | Understanding avg #warnings                                                                | 1                                                                                                                                                                                     | Number         |
|             | Errors                                  | Array of Error Classes (compile-time-constant)                                | Understand most common errors users encounter                                              | `["UnknownLanguage", "MaxFileSizeExceeded"] `                                                                                                                                         | ErrorClass[]   |
|             | Warnings                                | Array of Warning Classes (compile-time-constant)                              | Understand most common warnings users encounter                                            | `["TimeoutExceeded"]`                                                                                                                                                                 | WarningClass[] |
|             |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
| Value       |                                         |                                                                               |                                                                                            |                                                                                                                                                                                       |                |
|             | Engine requested                        | The engine requested by the user                                              | Understand which engines are being used; debug engine-specific problems                    | `"Interfile"`                                                                                                                                                                         | str            |
|             | [Features used](#feature-usage)         | List of strings that identify Semgrep features used                           | Understand what features users find valuable, and what we could deprecate                  | `["language/python", "option/deep", "option/no-git-ignore", "key/metavariable-comparison"]`                                                                                           | Object         |
|             | Rule hashes with findings               | Map of rule hashes to number of findings                                      | Understand which rules are providing value to the user; diagnose high false-positive rates | `{"7c43c962dfdbc52882f80021e4d0ef2396e6a950867e81e5f61e68390ee9e166": 4}`                                                                                                             | Object         |
|             | Total Findings                          | Count of all findings                                                         | Understand if rules are super noisy for the user                                           | 7                                                                                                                                                                                     | Number         |
|             | Total Nosems                            | Count of all `nosem` annotations that tell semgrep to ignore a finding        | Understand if rules are super noisy for the user                                           | 3                                                                                                                                                                                     | Number         |

### Anonymous user ID

> `anonymous_user_id: "5f52484c-3f82-4779-9353-b29bbd3193b6"`

This ID is stored in the `~/.semgrep/settings.yml` file.
If the ID disappears, the next run will generate a new one randomly.
The Semgrep team uses this to answer the following questions:

- > How many people use a given rule/ruleset/snippet?

  This helps our security researchers assess their performance,
  and we're planning to make these numbers public for all rule authors in the community.

- > What percentage of users log in?"

  We use this to evaluate our success as we build new authenticated features on Semgrep App.

This ID will only ever be sent with Semgrep's metrics collection endpoint,
meaning it cannot be used to track users across the web.

### Feature usage

> `"features": ["language/python", "option/deep", "option/no-git-ignore", "key/metavariable-comparison"]`

Examples of such features are: languages scanned, CLI options passed, keys used in rules, or certain code paths reached, such as using an :include instruction in a .semgrepignore file.
These strings do NOT include user data or specific settings.
As an example, for `semgrep scan --output=secret.txt` Semgrep sends `"option/output"` but will NOT send `"option/output=secret.txt"`.

The list of features tracked as of June 2022 is:

- `language`: What languages were scanned
- `cli-flag`/`cli-envvar`: What options were configured (does NOT include their value)
- `config`: What method was used to retrieve rules (does NOT include any of the rule)
- `registry-query`: The value of a `--config r/foo.bar.baz` setting, limited to the first word (e.g. `r/foo..` in this example)
- `ruleset`: The value of a `--config p/foobar` setting
- `semgrepignore`: Whether an `:include` statement was used in a .semgrepignore file
- `subcommand`: What subcommand was used (e.g. `scan` or `ci`)

The Semgrep team uses this to answer the following questions:

- > How many people use a given feature?

  This guides our development,
  and lets us decide when and how to deprecate features.

- > How does feature usage affect finding counts, error counts, and performance?

  We use this to evaluate experimental features
  and understand their production-readiness.

> Engine requested (OSS, Pro, Interfile)

The engine requested is stored separately from the other features. This is the
engine indicated by the user through app toggles or CLI flags. We use this for
debugging as well as to understand which engines people are using.

### Sample metrics

This is a sample blob of the aggregate metrics described above:

```
{
    "started_at": "2021-05-10T21:05:06+00:00",
    "sent_at": "2021-05-10T21:05:09+00:00",
    "event_id": "222bcccd-9dc2-4d10-ac3a-5692460e77ee",
    "anonymous_user_id": "5f52484c-3f82-4779-9353-b29bbd3193b6",
    "environment": {
        "version": "0.51.0",
        "ci": "true",
        "configNamesHash": "ede96c41b57de3e857090fb3c486e69ad8efae3267bac4ac5fbc19dde7161094",
        "projectHash": "c65437265631ab2566802d4d90797b27fbe0f608dceeb9451b979d1671c4bc1a",
        "rulesHash": "b03e452f389e5a86e56426c735afef13686b3e396499fc3c42561f36f6281c43",
        "isAuthenticated": false
    },
    "performance": {
        "runTime": 37.1234233823,
        "numRules": 2,
        "numTargets": 573,
        "totalBytesScanned": 33938923,
        "ruleStats": [{
          "ruleHash": "7c43c962dfdbc52882f80021e4d0ef2396e6a950867e81e5f61e68390ee9e166",
          "parseTime": 0,
          "matchTime": 0.05480456352233887,
          "runTime": 0.20836973190307617,
          "bytesScanned": 0
        }],
        "fileStats": [{
          "size": 6725,
          "numTimesScanned": 147,
          "parseTime": 0.013289928436279297,
          "matchTime": 0.05480456352233887,
          "runTime": 0.20836973190307617
        }]
    },
    "parse_rate": {
      "python": {
        "num_targets": 102,
        "targets_parsed": 101,
        "num_bytes": 985123,
        "bytes_parsed": 993419
      },
      "ruby": {
        "num_targets": 12,
        "targets_parsed": 12,
        "num_bytes": 341027,
        "bytes_parsed": 341027
      }
    },
    "errors": {
        "returnCode": 1,
        "errors": ["UnknownLanguage"],
        "warnings": ["MaxFileSizeExceeded", "TimeoutExceeded"]
    },
    "value": {
        "ruleHashesWithFindings": {"7c43c962dfdbc52882f80021e4d0ef2396e6a950867e81e5f61e68390ee9e166": 4},
        "numFindings": 7,
        "numIgnored": 3,
        "features": ["language/python", "option/deep", "option/no-git-ignore", "key/metavariable-comparison"],
        "engineRequested": "OSS"
    }
}
```

## Data collected when explicitly requested

For Semgrep App users running `semgrep ci` while logged in,
data is sent to power your dashboard, notification, dependency search, and finding management features.
These data are ONLY sent when using `semgrep ci` in an App-connected mode
and are not sent when not logged in.

Three types of data are sent to Semgrep, Inc servers for this logged-in use case: scan data, findings data, and dependencies data.

**Scan data** provide information on the environment and performance of Semgrep.
They power dashboards, identify anomalies with the product, and are needed for billing.
The classes of scan data are:

- Project identity (e.g., name, URL)
- Scan environment (e.g., CI provider, OS)
- Author identity (e.g., committer email)
- Commit metadata (e.g., commit hash and timestamp)
- Review and review-requester identifying data (e.g., pull-request ID, branch, merge base, request author)
- Scan metadata, including type of scan and scan parameters (e.g., paths scanned and extensions of ignored files)
- Timing metrics (e.g., time taken to scan per-rule and per-path)
- Parse metrics (e.g., number of files targeted and parsed per-language)
- Semgrep environment (e.g., version, interpreter, timestamp)

**Findings data** are used to provide human readable content for notifications and integrations,
as well tracking results as new, fixed, or duplicate. The classes of findings data are:

- Check ID and metadata (as defined in the rule definition; e.g., OWASP category, message, severity)
- Code location, including file path, that triggered findings
- A one-way hash of a unique code identifier that includes the triggering code content
- Dependency name and version (only sent when using Semgrep Supply Chain)
- Source code is NOT collected

**Dependencies data** are used to power Dependency Search and License Compliance. The classes of
dependencies data are:

- Package name (e.g., lodash)
- Package version (e.g., 1.2.3)
- File path for lockfile (e.g., frontend/yarn.lock)

## Registry fetches

Certain Registry resources require log-in to the Semgrep Registry. Log in may be performed
using your project URL, or a Semgrep.dev API token. When using these resources, your project's
identity will be recorded by the Semgrep Registry servers.

## Data sharing

We use some third party companies and services to help administer and provide Semgrep, for example for hosting, customer support, product usage analytics, and database management. These third parties are permitted to handle data only to perform these tasks in a manner consistent with this document and are obligated not to disclose or use it for any other purpose.

We do not share or sell the information provided to us with other organizations without explicit consent, except as described in this document.
