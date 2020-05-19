import json
import subprocess
from pathlib import Path

import pytest

pytestmark = pytest.mark.qa
SENTINEL_VALUE = 87518275812375164


def xfail_repo(url, *, reason=None):
    return pytest.param(url, marks=pytest.mark.xfail(reason=reason))


@pytest.mark.parametrize(
    "repo_url",
    [
        "https://github.com/Airtable/airtable.js",
        "https://github.com/seemoo-lab/opendrop",
        "https://github.com/lightstep/lightstep-tracer-python",
        "https://github.com/draios/sysdig-inspect",
        "https://github.com/getsentry/sentry-python",
        "https://github.com/signalapp/signal-webrtc-ios",
        "https://github.com/secdev/scapy",
        "https://github.com/apache/airflow",
        "https://github.com/preset-io/elasticsearch-dbapi",
        "https://github.com/apache/libcloud",
        "https://github.com/keybase/pykeybasebot",
        "https://gitbox.apache.org/repos/asf/cassandra",
        "https://github.com/coinbase/coinbase-commerce-python",
        "https://github.com/keybase/python-triplesec",
        "https://github.com/psycopg/psycopg2",
        "https://github.com/preset-io/flask-jwt-extended",
        "https://github.com/vstinner/pyperf",
        "https://github.com/mysql/mysql-connector-python",
        "https://github.com/Netflix/lemur",
        xfail_repo("https://github.com/highcharts/highcharts"),
        xfail_repo(
            "https://github.com/lodash/lodash",
            reason="https://github.com/returntocorp/semgrep/issues/580",
        ),
        xfail_repo("https://github.com/signalapp/Signal-Desktop"),
        xfail_repo("https://github.com/opensourceactivismtech/call-power"),
        xfail_repo("https://github.com/zulip/zulip"),
        xfail_repo(
            "https://github.com/home-assistant/home-assistant",
            reason=(
                "https://github.com/returntocorp/semgrep/issues/599, "
                "https://github.com/returntocorp/semgrep/issues/600, "
                "https://github.com/returntocorp/semgrep/issues/601, "
                "https://github.com/returntocorp/semgrep/issues/602"
            ),
        ),
        xfail_repo(
            "https://github.com/apache/incubator-superset",
            reason=(
                "https://github.com/returntocorp/semgrep/issues/581, "
                "https://github.com/returntocorp/semgrep/issues/582"
            ),
        ),
    ],
)
def test_semgrep_on_repo(monkeypatch, tmp_path, repo_url):
    TESTS_PATH = Path(__file__).parent.parent
    monkeypatch.setenv("PYTHONPATH", str(TESTS_PATH.parent.resolve()))
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "qa" / "rules").resolve())
    monkeypatch.chdir(tmp_path)

    subprocess.check_output(
        [
            "git",
            "clone",
            "--depth=1",
            "https://github.com/returntocorp/semgrep-rules",
            "repo",
        ]
    )
    languages = {
        "python": "py",
        "javascript": "js",
    }
    for language, file_ext in languages.items():
        sentinel_path = Path("repo") / f"sentinel.{file_ext}"
        with sentinel_path.open("w") as sentinel_file:
            sentinel_file.write(f"x = {SENTINEL_VALUE}")

        semgrep_run = subprocess.run(
            [
                "python3",
                "-m",
                "semgrep",
                "--pattern",
                f"$X = {SENTINEL_VALUE}",
                "--lang",
                language,
                "--json",
                "repo",
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            encoding="utf-8",
        )

        assert semgrep_run.returncode == 0
        try:
            output = json.loads(semgrep_run.stdout)
        except json.JSONDecodeError:
            pytest.fail(
                f"Failed to parse JSON from semgrep output:\n"
                + semgrep_run.stdout
                + semgrep_run.stderr
            )

        assert output["errors"] == []
        assert len(output["results"]) == 1
        assert output["results"][0]["path"] == str(sentinel_path)

    output = subprocess.check_output(
        [
            "python3",
            "-m",
            "semgrep",
            "--config=rules/regex-sentinel.yaml",
            "--strict",
            "--json",
            "repo",
        ],
        encoding="utf-8",
    )
    output = json.loads(output)
    assert len(output["results"]) == 2
    assert len(output["errors"]) == 0
