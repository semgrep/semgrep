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
- Git commit hashes, messages, authors
- User-identifiable data about Semgrep’s findings in your code, including finding messages

This data will be sent to Semgrep App only if you explicitly request it,
such as with `semgrep login && semgrep ci` to connect with Semgrep App.
Even in that case, your source code and private rules will never be sent.

## Data collected as metrics

Semgrep collects data to improve the user experience.
Four types of data are collected:

### Environmental

Environmental data provide contextual data about Semgrep’s runtime environment, as well as information that helps debug any issues users may be facing; e.g.

- How long the command took to run
- The version of Semgrep
- Value of the CI environment variable, if set
- Pseudoanonymized hash of the scanned project’s name
- Pseudoanonymized hash of the rule definitions run
- Pseduoanonymized hash of the config option

### Performance

Performance data enable understanding of which rules and types of files are slow in the aggregate so r2c can improve the Semgrep program-analysis engine, query optimizer, and debug slow rules; e.g.

- Runtime duration
- Total number of rules
- Total number of files
- Project size in bytes

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

### Pseudoanonymization

Certain identifying data (e.g. project URLs) are pseudoanonymized before being sent to the r2c backend.

"Pseudoanonymized" means the data are transformed using a deterministic cryptographically secure hash. When the input data are unknown, this hash is expensive to reverse. However, when input data are known, a reverse dictionary of identifiers to hashes can be built. Hence, data are anonymous only when the source values are unknown.

We use a deterministic hash to:

- Track performance and value improvements over successive runs on projects
- Remove test data from our metrics

Using a deterministic hash, however, implies:

- An entity that independently knows the value of an input datum AND who has access to r2c's metrics data could access metrics for that known datum

r2c will:

- Treat collected metrics data as secret, using application-security best practices, including (but not limited to)
  - Encryption during transit and rest
  - Strict access control to data-storage systems
  - Application-security-policy requirements for third parties (e.g. cloud-service providers; see "data sharing" below)
- Only correlate hashed data to input data when these inputs are already known to r2c (e.g. publicly available project URLs for open-source projects, or projects that log in to the Semgrep Registry)

## Description of metrics fields

| Category    | Field                     | Description                                                            | Use Case                                                                                   | Example Datum                                                                                                                                                                         | Type           |
| ----------- | ------------------------- | ---------------------------------------------------------------------- | ------------------------------------------------------------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------- |
| Environment |                           |                                                                        |                                                                                            |                                                                                                                                                                                       |                |
|             | Timestamps (started/sent) | Time when the event fired                                              | Understanding tool usage over time                                                         | 2021-05-10T21:05:06+00:00                                                                                                                                                             | String         |
|             | Version                   | Semgrep version being used                                             | Reproduce and debug issues with specific versions                                          | 0.51.0                                                                                                                                                                                | String         |
|             | Project URL               | Project URL **(sent only if config=auto)**                             | Fetch pre-configured rules for the org or project by name                                  | `git@github.com:returntocorp/semgrep.git`                                                                                                                                             | String         |
|             | Project hash              | One-way hash of the project URL                                        | Understand performance and accuracy improvements                                           | `c65437265631ab2566802d4d90797b27fbe0f608dceeb9451b979d1671c4bc1a`                                                                                                                    | String         |
|             | Rules hash                | One-way hash of the rule definitions                                   | Understand performance improvements                                                        | `b03e452f389e5a86e56426c735afef13686b3e396499fc3c42561f36f6281c43`                                                                                                                    | String         |
|             | Config hash               | One-way hash of the config argument                                    | Understand performance and accuracy improvements                                           | `ede96c41b57de3e857090fb3c486e69ad8efae3267bac4ac5fbc19dde7161094`                                                                                                                    | String         |
|             | Is authenticated          | Whether the user is logged in                                          | Understand popularity of logged in features                                                | `false`                                                                                                                                                                               | Boolean        |
|             | CI                        | Notes if Semgrep is running in CI and the name of the provider         | Reproduce and debug issues with specific CI providers                                      | GitLabCI v0.13.12                                                                                                                                                                     | String         |
|             |                           |                                                                        |                                                                                            |                                                                                                                                                                                       |                |
| Performance |                           |                                                                        |                                                                                            |                                                                                                                                                                                       |                |
|             | Duration                  | How long the command took to run                                       | Understanding aggregate performance improvements and regressions                           | 14.13                                                                                                                                                                                 | Number         |
|             | Total Rules               | Count of rules                                                         | Understand how duration is affected by #rules                                              | 137                                                                                                                                                                                   | Number         |
|             | Total Files               | Count of files                                                         | Understand how duration is affected by #files                                              | 4378                                                                                                                                                                                  | Number         |
|             | Total Bytes               | Summation of target file size                                          | Understand how duration is related to total size of all target files                       | 40838239                                                                                                                                                                              | Number         |
|             | Rule Stats                | Performance statistics (w/ rule hashes) for slowest rules              | Debug rule performance                                                                     | `[{"ruleHash": "7c43c962dfdbc52882f80021e4d0ef2396e6a950867e81e5f61e68390ee9e166","parseTime": 0,"matchTime": 0.05480456352233887,"runTime": 0.20836973190307617,"bytesScanned": 0}]` | StatsClass[]   |
|             | File Stats                | Performance statistics for slowest files                               | Debug rule performance                                                                     | `[{"size": 6725,"numTimesScanned": 147,"parseTime": 0.013289928436279297,"matchTime": 0.05480456352233887,"runTime": 0.20836973190307617}]`                                           | StatsClass[]   |
|             |                           |                                                                        |                                                                                            |                                                                                                                                                                                       |                |
| Errors      |                           |                                                                        |                                                                                            |                                                                                                                                                                                       |                |
|             | Exit Code                 | Numeric exit code                                                      | Debug commonly occurring issues and aggregate error counts                                 | 1                                                                                                                                                                                     | Number         |
|             | Number of Errors          | Count of errors                                                        | Understanding avg #errors                                                                  | 2                                                                                                                                                                                     | Number         |
|             | Number of Warnings        | Count of warnings                                                      | Understanding avg #warnings                                                                | 1                                                                                                                                                                                     | Number         |
|             | Errors                    | Array of Error Classes (compile-time-constant)                         | Understand most common errors users encounter                                              | `["UnknownLanguage", "MaxFileSizeExceeded"] `                                                                                                                                         | ErrorClass[]   |
|             | Warnings                  | Array of Warning Classes (compile-time-constant)                       | Understand most common warnings users encounter                                            | `["TimeoutExceeded"]`                                                                                                                                                                 | WarningClass[] |
|             |                           |                                                                        |                                                                                            |                                                                                                                                                                                       |                |
| Value       |                           |                                                                        |                                                                                            |                                                                                                                                                                                       |                |
|             | Rule hashes with findings | Map of rule hashes to number of findings                               | Understand which rules are providing value to the user; diagnose high false-positive rates | `{"7c43c962dfdbc52882f80021e4d0ef2396e6a950867e81e5f61e68390ee9e166": 4}`                                                                                                             | Object         |
|             | Total Findings            | Count of all findings                                                  | Understand if rules are super noisy for the user                                           | 7                                                                                                                                                                                     | Number         |
|             | Total Nosems              | Count of all `nosem` annotations that tell semgrep to ignore a finding | Understand if rules are super noisy for the user                                           | 3                                                                                                                                                                                     | Number         |

### Sample metrics

This is a sample blob of the aggregate metrics described above:

```
{
    "environment": {
        "version": "0.51.0",
        "ci": "true",
        "configNamesHash": "ede96c41b57de3e857090fb3c486e69ad8efae3267bac4ac5fbc19dde7161094",
        "projectHash": "c65437265631ab2566802d4d90797b27fbe0f608dceeb9451b979d1671c4bc1a",
        "rulesHash": "b03e452f389e5a86e56426c735afef13686b3e396499fc3c42561f36f6281c43",
        "isAuthenticated": false,
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
    "errors": {
        "returnCode": 1,
        "errors": ["UnknownLanguage"],
        "warnings": ["MaxFileSizeExceeded", "TimeoutExceeded"]
    },
    "value": {
        "ruleHashesWithFindings": {"7c43c962dfdbc52882f80021e4d0ef2396e6a950867e81e5f61e68390ee9e166": 4},
        "numFindings": 7,
        "numIgnored": 3
    }
}
```

## Data collected when explicitly requested

For Semgrep App users running `semgrep ci` while logged in,
data is sent to power your dashboard, notification, and finding management features.
These data are ONLY sent when using `semgrep ci` in an App-connected mode
and are not sent when not logged in.

Two types of data are sent to r2c servers for this logged-in use case: scan data and findings data.

**Scan data** provide information on the environment and performance of Semgrep.
They power dashboards, identify anomalies with the product, and are needed for billing.
The classes of scan data are:

- Project identity (e.g., name, URL)
- Scan environment (e.g., CI provider, OS)
- Author identity (e.g., committer email)
- Commit metadata (e.g., commit hash)
- Review and review-requester identifying data (e.g., pull-request ID, branch, merge base, request author)
- Scan metadata, including type of scan and scan parameters (e.g., paths scanned)
- Timing metrics (e.g., time taken to scan per-rule and per-path)
- Semgrep environment (e.g., version, interpreter, timestamp)

**Findings data** are used to provide human readable content for notifications and integrations,
as well tracking results as new, fixed, or duplicate. The classes of findings data are:

- Check ID and metadata (as defined in the rule definition; e.g., OWASP category, message, severity)
- Code location, including file path, that triggered findings
- A one-way hash of a unique code identifier that includes the triggering code content
- Source code is NOT collected

## Registry fetches

Certain Registry resources require log-in to the Semgrep Registry. Log in may be performed
using your project URL, or a Semgrep.dev API token. When using these resources, your project's
identity will be recorded by the Semgrep Registry servers.

## Data sharing

We use some third party companies and services to help administer and provide Semgrep, for example for hosting, customer support, product usage analytics, and database management. These third parties are permitted to handle data only to perform these tasks in a manner consistent with this document and are obligated not to disclose or use it for any other purpose.

We do not share or sell the information provided to us with other organizations without explicit consent, except as described in this document.
