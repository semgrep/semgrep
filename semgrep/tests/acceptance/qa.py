import json
import os
import re
import shutil
import subprocess
import sys
import tempfile
from contextlib import contextmanager
from pathlib import Path
from typing import Any
from typing import Callable
from typing import Iterator
from typing import List
from typing import Mapping
from typing import Optional

import yaml


@contextmanager
def git_checkout(url: str, commit_hash: Optional[str] = None) -> Iterator[str]:
    """
        Clones URL into destination

        Returns name of directory url was cloned into
    """
    with tempfile.TemporaryDirectory() as destination:
        subprocess.run(
            ["git", "clone", "--depth=1", url, destination],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=True,
        )
        yield destination


def run_repo(target: str, rewrite: bool = False) -> None:
    """
    Runs semgrep on github repo with a simple pattern

    Check that semgrep did not into any errors parsing targets
    """
    with git_checkout(target) as target_dir:
        command = [
            "python3",
            "-m",
            "semgrep",
            "--pattern=$X==$X",
            "--lang=python",
            "--lang=javascript",
            "--json",
            f"{Path(target_dir).resolve()}",
        ]

        runned = subprocess.run(
            command, stdout=subprocess.PIPE, stderr=subprocess.PIPE, encoding="utf-8",
        )

        try:
            output = json.loads(runned.stdout)
        except ValueError:
            print("Output was not JSON: ")
            print(runned.stdout)
            # raise
        assert "results" in output
        print(output["errors"])
        assert len(output["errors"]) == 0


def test_airtablejs() -> None:
    run_repo("https://github.com/Airtable/airtable.js")


def test_opendrop() -> None:
    run_repo("https://github.com/seemoo-lab/opendrop")


def test_lightstep_tracer_python() -> None:
    run_repo("https://github.com/lightstep/lightstep-tracer-python")


def test_sysdig_inspect() -> None:
    run_repo("https://github.com/draios/sysdig-inspect")


def test_sentry_python() -> None:
    run_repo("https://github.com/getsentry/sentry-python")


def test_signal_webrtc_ios() -> None:
    run_repo("https://github.com/signalapp/signal-webrtc-ios")


def test_scapy() -> None:
    run_repo("https://github.com/secdev/scapy")


def test_airflow() -> None:
    run_repo("https://github.com/apache/airflow")


def test_elasticsearch_dbapi() -> None:
    run_repo("https://github.com/preset-io/elasticsearch-dbapi")


def test_libcloud() -> None:
    run_repo("https://github.com/apache/libcloud")


def test_pykeybasebot() -> None:
    run_repo("https://github.com/keybase/pykeybasebot")


def test_cassandra() -> None:
    run_repo("https://gitbox.apache.org/repos/asf/cassandra")


def test_coinbase_commerce_python() -> None:
    run_repo("https://github.com/coinbase/coinbase-commerce-python")


def test_python_triplesec() -> None:
    run_repo("https://github.com/keybase/python-triplesec")


def test_psycopg2() -> None:
    run_repo("https://github.com/psycopg/psycopg2")


def test_flask_jwt_extended() -> None:
    run_repo("https://github.com/preset-io/flask-jwt-extended")


def test_pyperf() -> None:
    run_repo("https://github.com/vstinner/pyperf")


def test_mysql_connector_python() -> None:
    run_repo("https://github.com/mysql/mysql-connector-python")


def test_lemur() -> None:
    run_repo("https://github.com/Netflix/lemur")


# # fail
# def test_highcharts() -> None:
#     run_repo("https://github.com/highcharts/highcharts")


# # fail uncomment after #580
# def test_lodash() -> None:
#     run_repo("https://github.com/lodash/lodash")


# # fail
# def test_Signal_Desktop() -> None:
#     run_repo("https://github.com/signalapp/Signal-Desktop")


# # fail
# def test_call_power() -> None:
#     run_repo("https://github.com/opensourceactivismtech/call-power")


# # fail
# def test_zulip() -> None:
#     run_repo("https://github.com/zulip/zulip")


# # fail uncomment after #599 #600 #601 #602
# def test_home_assistant() -> None:
#     run_repo("https://github.com/home-assistant/home-assistant")


# # fail uncomment after #581 #582
# def test_incubator_superset() -> None:
#     run_repo("https://github.com/apache/incubator-superset")
