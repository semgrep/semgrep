# Semgrep Privacy Policy

Semgrep may collect non-identifiable aggregate metrics to help improve the product. This document describes:

* the principles that guide our data-collection decisions
* how to opt-in to Semgrep’s metrics
* the breakdown of the data that are and are not collected
* how we use the data to make Semgrep better

## Principles

These principles inform our decisions around data collection:

1. **Transparency**: Collect and use data in a way that is clearly explained to the user and benefits them
2. **User control**: Put users in control of their data at all times
3. **Limited data**: Collect what is needed, de-identify where possible, and delete when no longer necessary

## Opt-in behavior

Semgrep’s opt-in aggregate metrics are only sent when the environment variable `SEMGREP_SEND_METRICS` is set or when the flag `--enable-metrics` is set. If this environment variable is not set or if the flag is not set, the aggregate metrics are not sent anywhere.

## Collected data

Semgrep collects non-identifiable data to improve the user experience. Four types of data are collected:

### Environmental

Environmental data provides contextual data about Semgrep’s runtime environment, as well as information that helps debug any issues users may be facing; e.g.

* How long the command took to run
* If the command ran in a CI environment
* The version of Semgrep
* The user’s OS and shell
* Anonymized hash of the scanned project’s name
* Anonymized hash of the rules run

### Performance

Performance data enables understanding of which rules and types of files are slow in the aggregate so r2c can improve the Semgrep program-analysis engine, query optimizer, and debug slow rules; e.g.

* Runtime duration
* Total number of rules
* Total number of files
* Project size in bytes
* One-way hashes of the rule definitions

### Errors

High-level error and warning classes that Semgrep encounters when run; e.g.

* Semgrep’s return code
* The number of errors
* Compile-time error names, e.g., MaxFileSizeExceeded, SystemOutOfMemory, UnknownFileEncoding

### Value

Semgrep reports data that indicate how useful a run is for the end user; e.g.

* Number of raised findings
* Number of ignored findings
* One-way hashes of the rule definitions that yield findings

### Data NOT collected

We strive to balance our desire to collect data for improving Semgrep with our users' need for privacy. The following items don't leave your environment and are not sent or shared with anyone.

* Source code
* Raw repository names, filenames, file contents, or commit hashes
* User-identifiable data about Semgrep’s findings in your code, including finding messages
* Private rules



## Description of fields

|Category   |Field  |Description    |Use Case   |Example Data   |Type   |
|---    |---    |---    |---    |---    |---    |
|Environment    |   |   |   |   |   |
|   |Timestamp  |Time when the event fired  |Understanding tool usage over time |2021-05-10T21:05:06+00:00  |String |
|   |Version    |Semgrep version being used |Reproduce and debug issues with specific versions  |0.51.0 |String |
|   |CI |Notes if Semgrep is running in CI and the name of the provider |Reproduce and debug issues with specific CI providers  |GitLabCI v0.13.12  |String |
|   |   |   |   |   |   |
|Performance    |   |   |   |   |   |
|   |Duration   |How long the command took to run   |Understanding agregate performance improvements and regressions    |14.13  |Number |
|   |Total Rules    |Count of rules |Understand how duration is affected by #rules  |137    |Number |
|   |Total Files    |Count of files |Understand how duration is affected by #files  |4378   |Number |
|   |Total Bytes    |Summation of target file size  |Understand how duration is related to total size of all target files   |40838239   |Number |
|   |   |   |   |   |   |
|Errors |   |   |   |   |   |
|   |Exit Code  |Numeric exit code  |Debug commonly occurring issues and aggregate error counts |1  |Number |
|   |Number of Errors   |Count of errors    |Understanding avg #errors  |2  |Number |
|   |Number of Warnings |Count of warnings  |Understanding avg #warnings    |1  |Number |
|   |Errors |Array of Error Classes (compile-time-constant) |Understand most common errors users encounter  |["UnknownLanguage", "MaxFileSizeExceeded"] |ErrorClass[]   |
|   |Warnings   |Array of Warning Classes (compile-time-constant)   |Understand most common warnings users encounter    |["TimeoutExceeded"]    |WarningClass[] |
|   |   |   |   |   |   |
|Value  |   |   |   |   |   |
|   |Total Findings |Count of all findings  |Understand if rules are super noisy for the user   |7  |Number |
|   |Total Nosems   |Count of all `nosem` annotations that tell semgrep to ignore a finding |Understand if rules are super noisy for the user   |3  |Number |



### Sample metrics

This is a sample blob of the non-identifiable aggregate metrics described above:

```
{
    "environment": {
        "timestamp": "2021-05-10T21:05:06+00:00",
        "version": "0.51.0",
        "ci": "Gitlab v13.12"
    },
    "performance": {
        "duration": 37.1234233823,
        "totalRules": 2,
        "totalFiles": 573,
        "totalBytes": 33938923
    },
    "errors": {
        "exitCode": 1,
        "totalErrors": 1,
        "totalWarnings": 2,
        "errors": ["UnknownLanguage"],
        "warnings": ["MaxFileSizeExceeded", "TimeoutExceeded"]
    },
    "value": {
        "totalFindings": 7,
        "totalNosems": 3
    }
}
```



## Data sharing

We use some third party companies and services to help administer and provide Semgrep, for example for hosting, customer support, product usage analytics, and database management. These third parties are permitted to handle data only to perform these tasks in a manner consistent with this document and are obligated not to disclose or use it for any other purpose.

We do not share or sell the information provided to us with other organizations without explicit consent, except as described in this document.
