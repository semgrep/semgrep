import json
import subprocess


def test_synthesize_patterns():
    output = subprocess.check_output(
        [
            "python",
            "-m",
            "semgrep",
            "--disable-version-check",
            "--metrics",
            "off",
            "--synthesize-patterns",
            "6:10-6:30",
            "--lang",
            "py",
            "tests/e2e/rules/synthesizing/ex1.py",
        ],
        encoding="utf-8",
    )
    print("output = ", output)
    output_json = json.loads(output)
    assert output_json["exact match"] == "metrics.send('my-report-id')"
    assert output_json["dots"] == "metrics.send(...)"
