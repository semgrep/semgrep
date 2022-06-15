import json

import pytest


@pytest.mark.kinda_slow
def test_synthesize_patterns(run_semgrep_in_tmp):
    stdout, _ = run_semgrep_in_tmp(
        target_name="synthesizing/ex1.py",
        options=[
            "--synthesize-patterns=6:10-6:30",
            "--lang=py",
        ],
    )
    output_json = json.loads(stdout)
    assert output_json["exact match"] == "metrics.send('my-report-id')"
    assert output_json["dots"] == "metrics.send(...)"
