import json
import subprocess
from pathlib import Path

import pytest

pytestmark = pytest.mark.qa


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
def test_semgrep_on_repo(run_semgrep_in_tmp, repo_url):
    subprocess.check_output(
        [
            "git",
            "clone",
            "--depth=1",
            "https://github.com/returntocorp/semgrep-rules",
            "repo",
        ]
    )
    semgrep_run = subprocess.run(
        [
            "python3",
            "-m",
            "semgrep",
            "--pattern",
            "$X == $X",
            "--lang=python",
            "--lang=javascript",
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

    assert "results" in output
