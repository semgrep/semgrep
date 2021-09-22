# Semgrep Privacy Policy

Semgrep may collect aggregate metrics to help improve the product. This document describes:

* the principles that guide our data-collection decisions
* how to opt-in to Semgrep’s metrics
* the breakdown of the data that are and are not collected
* how we use the data to make Semgrep better

## Principles

These principles inform our decisions around data collection:

1. **Transparency**: Collect and use data in a way that is clearly explained to the user and benefits them
2. **User control**: Put users in control of their data at all times
3. **Limited data**: Collect what is needed, pseduoanonymize where possible, and delete when no longer necessary

## Opt-in behavior

Semgrep’s opt-in aggregate metrics are only sent when the environment variable `SEMGREP_SEND_METRICS` is set or when the flag `--enable-metrics` is set. If this environment variable is not set or if the flag is not set, the aggregate metrics are not sent anywhere.

Note that certain Semgrep integrators set this environment variable by default. For example, both the [Semgrep CI agent](https://github.com/returntocorp/semgrep-action) and [GitLab's Semgrep SAST analyzer](https://gitlab.com/gitlab-org/security-products/analyzers/semgrep) enable aggregate-metrics collection by default.

## Collected data

Semgrep collects data to improve the user experience. Four types of data are collected:

### Environmental

Environmental data provide contextual data about Semgrep’s runtime environment, as well as information that helps debug any issues users may be facing; e.g.

* How long the command took to run
* The version of Semgrep
* Pseudoanonymized hash of the scanned project’s name
* Pseudoanonymized hash of the rule definitions run
* Pseduoanonymized hash of the config option

### Performance

Performance data enable understanding of which rules and types of files are slow in the aggregate so r2c can improve the Semgrep program-analysis engine, query optimizer, and debug slow rules; e.g.

* Runtime duration
* Total number of rules
* Total number of files
* Project size in bytes

### Errors

High-level error and warning classes that Semgrep encounters when run; e.g.

* Semgrep’s return code
* The number of errors
* Compile-time error names, e.g., MaxFileSizeExceeded, SystemOutOfMemory, UnknownFileEncoding

### Value

Semgrep reports data that indicate how useful a run is for the end user; e.g.

* Number of raised findings
* Number of ignored findings
* Pseudoanonymized hashes of the rule definitions that yield findings

### Data NOT collected

We strive to balance our desire to collect data for improving Semgrep with our users' need for privacy. The following items don't leave your environment and are not sent or shared with anyone.

* Source code
* Raw repository names, filenames, file contents, or commit hashes
  * *Except* the raw repository name is sent to Semgrep when using `--config
    auto`
* User-identifiable data about Semgrep’s findings in your code, including finding messages
* Private rules

### Pseudoanonymization

Certain identifying data (e.g. project URLs) are pseudoanonymized before being sent to the r2c backend.

"Pseudoanonymized" means the data are transformed using a deterministic cryptographically secure hash. When the input data are unknown, this hash is expensive to reverse. However, when input data are known, a reverse dictionary of identifiers to hashes can be built. Hence, data are anonymous only when the source values are unknown.

We use a deterministic hash to:
* Track performance and value improvements over succesive runs on projects
* Remove test data from our metrics

Using a deterministic hash, however, implies:
* An entity that independently knows the value of an input datum AND who has access to r2c's metrics data could access metrics for that known datum

r2c will:
* Treat collected metrics data as secret, using application-security best practices, including (but not limited to)
  * Encryption during transit and rest
  * Strict access control to data-storage systems
  * Application-security-policy requirements for third parties (e.g. cloud-service providers; see "data sharing" below)
* Only correlate hashed data to input data when these inputs are already known to r2c (e.g. publicly available project URLs for open-source projects, or projects that log in to the Semgrep Registry)


## Description of fields

|Category   |Field  |Description    |Use Case   |Example Datum   |Type   |
|---    |---    |---    |---    |---    |---    |
|Environment    |   |   |   |   |   |
|   |Timestamp  |Time when the event fired                              |Understanding tool usage over time                     |2021-05-10T21:05:06+00:00  |String |
|   |Version    |Semgrep version being used                             |Reproduce and debug issues with specific versions      |0.51.0 |String |
|   |Project hash |One-way hash of the project URL                      |Understand performance and accuracy improvements      |`c65437265631ab2566802d4d90797b27fbe0f608dceeb9451b979d1671c4bc1a`|String |
|   |Rules hash |One-way hash of the rule definitions                   |Understand performance improvements                    |`b03e452f389e5a86e56426c735afef13686b3e396499fc3c42561f36f6281c43`|String |
|   |Config hash|One-way hash of the config argument                    |Understand performance and accuracy improvements       |`ede96c41b57de3e857090fb3c486e69ad8efae3267bac4ac5fbc19dde7161094`|String |
|   |CI |Notes if Semgrep is running in CI and the name of the provider |Reproduce and debug issues with specific CI providers  |GitLabCI v0.13.12  |String |
|   |   |   |   |   |   |
|Performance    |   |   |   |   |   |
|   |Duration   |How long the command took to run   |Understanding agregate performance improvements and regressions    |14.13  |Number |
|   |Total Rules    |Count of rules |Understand how duration is affected by #rules  |137    |Number |
|   |Total Files    |Count of files |Understand how duration is affected by #files  |4378   |Number |
|   |Total Bytes    |Summation of target file size  |Understand how duration is related to total size of all target files   |40838239   |Number |
|   |Rule Stats |Performance statistics (w/ rule hashes) for slowest rules|Debug rule performance|`[{"ruleHash": "7c43c962dfdbc52882f80021e4d0ef2396e6a950867e81e5f61e68390ee9e166","parseTime": 0,"matchTime": 0.05480456352233887,"runTime": 0.20836973190307617,"bytesScanned": 0}]`|StatsClass[] |
|   |File Stats |Performance statistics for slowest files|Debug rule performance|`[{"size": 6725,"numTimesScanned": 147,"parseTime": 0.013289928436279297,"matchTime": 0.05480456352233887,"runTime": 0.20836973190307617}]`|StatsClass[] |
|   |   |   |   |   |   |
|Errors |   |   |   |   |   |
|   |Exit Code  |Numeric exit code  |Debug commonly occurring issues and aggregate error counts |1  |Number |
|   |Number of Errors   |Count of errors    |Understanding avg #errors  |2  |Number |
|   |Number of Warnings |Count of warnings  |Understanding avg #warnings    |1  |Number |
|   |Errors |Array of Error Classes (compile-time-constant) |Understand most common errors users encounter  |`["UnknownLanguage", "MaxFileSizeExceeded"] `|ErrorClass[]   |
|   |Warnings   |Array of Warning Classes (compile-time-constant)   |Understand most common warnings users encounter    |`["TimeoutExceeded"]`    |WarningClass[] |
|   |   |   |   |   |   |
|Value  |   |   |   |   |   |
|   |Total Findings |Count of all findings  |Understand if rules are super noisy for the user   |7  |Number |
|   |Total Nosems   |Count of all `nosem` annotations that tell semgrep to ignore a finding |Understand if rules are super noisy for the user   |3  |Number |



### Sample metrics

This is a sample blob of the aggregate metrics described above:

```
{
    "environment": {
        "version": "0.51.0",
        "configNamesHash": "ede96c41b57de3e857090fb3c486e69ad8efae3267bac4ac5fbc19dde7161094",
        "projectHash": "c65437265631ab2566802d4d90797b27fbe0f608dceeb9451b979d1671c4bc1a",
        "rulesHash": "b03e452f389e5a86e56426c735afef13686b3e396499fc3c42561f36f6281c43",
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
        "numFindings": 7,
        "numIgnored": 3
    }
}
```

## Registry fetches

Certain Registry resources require log-in to the Semgrep Registry. Log in may be performed
using your project URL, or a Semgrep.dev API token. When using these resources, your project's
identity will be recorded by the Semgrep Registry servers.

## Data sharing

We use some third party companies and services to help administer and provide Semgrep, for example for hosting, customer support, product usage analytics, and database management. These third parties are permitted to handle data only to perform these tasks in a manner consistent with this document and are obligated not to disclose or use it for any other purpose.

We do not share or sell the information provided to us with other organizations without explicit consent, except as described in this document.
