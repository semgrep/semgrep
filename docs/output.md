# Output

This document describes `semgrep` output and the information provided after
running the program. By default output is sent to `stdout` in a user-readable
format. JSON output may also be specified with the `--json` flag.

Contents:

* [Default](#default)
* [JSON](#json)

## Default

By default, `semgrep` outputs results to `stdout`. The output looks like:

```
<finding-file-path>
rule:<rule-id>: <rule-message>
<finding-line-number>: <finding-line-code>
```

The `<rule-id>` and `<rule-message>` should be familiar from the [configuration
file fields](configuration-files.md#schema). The `<finding-file>`,
`<finding-line-number>`, and `<finding-line-code>` are included to conveniently
describe the context around the finding.

The following is example output from an [r2c rule](https://github.com/returntocorp/semgrep-rules):

```
node.py
rule:python.deadcode.eqeq-is-bad: useless comparison operation `node.id == node.id` or `node.id != node.id`.
3:        if node.id == node.id:  # Oops, supposed to be 'node_id'
```

## JSON

JSON output can be specified with the `--json` flag. This is useful for hooking
`semgrep`'s findings into other programs or tools. This form of output is much
more verbose and provides the full context around a finding.

JSON output looks like:

```
{
    "results": [
        {
            "check_id": <rule-id>,
            "path": <finding-file-path>,
            "extra": {
                "lines": <finding-line-code>,
                "message": <rule-message>,
                "metadata": {},
                "metavars": {
                    <metavariable-name>: {
                        "abstract_content": <metavariable-content>,
                        "start": {
                            "col": <finding-line-column-start>,
                            "line": <finding-line-number-start>,
                            "offset": <finding-byte-offset-start>
                        },
                        "end": {
                            "col": <finding-line-column-end>,
                            "line": <finding-line-number-end>,
                            "offset": <finding-byte-offset-end>
                        },
                        "unique_id": {
                            "md5sum": <finding-unique-idenfier>,
                            "type": "AST"|"id"
                        }
                    }
                },
                "severity": "WARNING"|"ERROR"
            },
            "start": {
                "col": <finding-line-column-start>,
                "line": <finding-line-number-start>
            },
            "end": {
                "col": <finding-line-column-end>,
                "line": <finding-line-number-end>
            }
        },
        {
            "check_id": <rule-id>,
            ...
        },
        ...
    ],
    "errors": [
        {
            "message": "SemgrepCoreRuntimeErrors",
            "data": <error-data>
        },
        ...
    ]
}
```

The following is example output from an [r2c rule](https://github.com/returntocorp/semgrep-rules):

```
{
    "results": [
        {
            "check_id": "python.deadcode.eqeq-is-bad",
            "path": "targets/basic/test.py",
            "extra": {
                "lines": "    return a + b == a + b",
                "message": "useless comparison operation `a+b == a+b` or `a+b != a+b`; if testing for floating point NaN, use `math.isnan`, or `cmath.isnan` if the number is complex.",
                "metadata": {},
                "metavars": {
                    "$X": {
                        "abstract_content": "a+b",
                        "start": {
                            "col": 12,
                            "line": 3,
                            "offset": 55
                        },
                        "end": {
                            "col": 17,
                            "line": 3,
                            "offset": 60
                        },
                        "unique_id": {
                            "md5sum": "07d71d85769e594dba9d7ae3d295c01f",
                            "type": "AST"
                        }
                    }
                },
                "severity": "ERROR"
            },
            "start": {
                "col": 12,
                "line": 3
            },
            "end": {
                "col": 26,
                "line": 3
            }
        }
    ],
    "errors": []
}
```
