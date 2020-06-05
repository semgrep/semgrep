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
        xfail_repo("https://github.com/coinbase/react-coinbase-commerce"),
        "https://github.com/coinbase/bifrost",
        "https://github.com/coinbase/bip38",
        "https://github.com/coinbase/bittip",
        "https://github.com/coinbase/btcexport",
        "https://github.com/coinbase/coinbase-android-sdk",
        "https://github.com/coinbase/coinbase-android-sdk-example",
        "https://github.com/coinbase/coinbase-bitmonet-sdk",
        "https://github.com/coinbase/coinbase-commerce-node",
        "https://github.com/coinbase/coinbase-commerce-python",
        "https://github.com/coinbase/coinbase-exchange-node",
        "https://github.com/coinbase/coinbase-java",
        "https://github.com/coinbase/coinbase-javascript-sdk",
        "https://github.com/coinbase/coinbase-node",
        "https://github.com/coinbase/coinbase-pro-node",
        "https://github.com/coinbase/coinbase-python",
        "https://github.com/coinbase/coinbase-tip-discourse",
        "https://github.com/coinbase/dexter",
        "https://github.com/coinbase/fenrir",
        "https://github.com/coinbase/gtt-ui",
        "https://github.com/coinbase/multisig-tool",
        "https://github.com/coinbase/node-process-lock",
        "https://github.com/coinbase/odin",
        "https://github.com/coinbase/protoc-gen-rbi",
        "https://github.com/coinbase/pwnbot",
        "https://github.com/coinbase/rosetta-cli",
        "https://github.com/coinbase/rosetta-sdk-go",
        "https://github.com/coinbase/self-service-iam",
        "https://github.com/coinbase/solidity-workshop",
        "https://github.com/coinbase/step",
        "https://github.com/coinbase/step-asg-deployer",
        "https://github.com/coinbase/watchdog",
        "https://github.com/dropbox/AskBox",
        "https://github.com/dropbox/Developer-Samples",
        xfail_repo("https://github.com/dropbox/DropboxBusinessScripts"),
        "https://github.com/dropbox/PyHive",
        xfail_repo("https://github.com/dropbox/changes"),
        xfail_repo("https://github.com/dropbox/changes-artifacts"),
        "https://github.com/dropbox/changes-client",
        "https://github.com/dropbox/changes-lxc-wrapper",
        "https://github.com/dropbox/changes-mesos-framework",
        "https://github.com/dropbox/dbx-unittest2pytest",
        xfail_repo("https://github.com/dropbox/dbx_build_tools"),
        "https://github.com/dropbox/dbxcli",
        "https://github.com/dropbox/dropbox-api-content-hasher",
        "https://github.com/dropbox/dropbox-api-v2-repl",
        "https://github.com/dropbox/dropbox-sdk-go-unofficial",
        "https://github.com/dropbox/dropbox-sdk-java",
        "https://github.com/dropbox/dropbox-sdk-js",
        "https://github.com/dropbox/dropbox-sdk-python",
        "https://github.com/dropbox/dropbox_hook",
        "https://github.com/dropbox/emmer",
        "https://github.com/dropbox/firebase-dropbox-oauth",
        "https://github.com/dropbox/gallerywithtagstutorial",
        "https://github.com/dropbox/git-rbr",
        xfail_repo("https://github.com/dropbox/godropbox"),
        "https://github.com/dropbox/goebpf",
        "https://github.com/dropbox/goprotoc",
        "https://github.com/dropbox/grallama-panel",
        "https://github.com/dropbox/groupy",
        xfail_repo("https://github.com/dropbox/hackpad"),
        "https://github.com/dropbox/hermes",
        "https://github.com/dropbox/hocrux",
        "https://github.com/dropbox/hydra",
        xfail_repo("https://github.com/dropbox/incubator-superset-internal"),
        "https://github.com/dropbox/llama",
        "https://github.com/dropbox/llama-archive",
        "https://github.com/dropbox/load_management",
        "https://github.com/dropbox/mdwebhook",
        "https://github.com/dropbox/merou",
        "https://github.com/dropbox/mypy-PyCharm-plugin",
        "https://github.com/dropbox/mypy-protobuf",
        "https://github.com/dropbox/nautilus-dropbox",
        "https://github.com/dropbox/nodegallerytutorial",
        "https://github.com/dropbox/notouch",
        "https://github.com/dropbox/nsot",
        "https://github.com/dropbox/othw",
        "https://github.com/dropbox/pem-converter-maven-plugin",
        "https://github.com/dropbox/pep8squad",
        "https://github.com/dropbox/presto-kafka-connector",
        "https://github.com/dropbox/puppet_run",
        "https://github.com/dropbox/pyannotate",
        "https://github.com/dropbox/pygerduty",
        "https://github.com/dropbox/pynsot",
        xfail_repo("https://github.com/dropbox/pyston"),
        xfail_repo("https://github.com/dropbox/pyston-perf"),
        xfail_repo("https://github.com/dropbox/pyston-testsuite"),
        "https://github.com/dropbox/pytest-call-tracer",
        "https://github.com/dropbox/pytest-flakefinder",
        "https://github.com/dropbox/python-invariant",
        xfail_repo("https://github.com/dropbox/pyxl"),
        "https://github.com/dropbox/questions",
        "https://github.com/dropbox/revision-browser",
        "https://github.com/dropbox/rules_node",
        "https://github.com/dropbox/securitybot",
        "https://github.com/dropbox/spookify",
        "https://github.com/dropbox/sqlalchemy-stubs",
        "https://github.com/dropbox/stone",
        "https://github.com/dropbox/stopwatch",
        "https://github.com/dropbox/strongpoc",
        xfail_repo("https://github.com/dropbox/trapperkeeper"),
        "https://github.com/dropbox/whitegold",
        "https://github.com/dropbox/ykfipsconf",
        "https://github.com/dropbox/zinger",
        "https://github.com/returntocorp/badwords",
        xfail_repo(
            "https://github.com/returntocorp/bento",
            reason="has intentionally unparsable code",
        ),
        "https://github.com/returntocorp/bento-report",
        "https://github.com/returntocorp/buffer-rule-tests",
        "https://github.com/returntocorp/check-docs",
        "https://github.com/returntocorp/cli",
        "https://github.com/returntocorp/flake8-click",
        "https://github.com/returntocorp/flake8-flask",
        "https://github.com/returntocorp/flake8-requests",
        "https://github.com/returntocorp/inputset-generator",
        xfail_repo(
            "https://github.com/returntocorp/semgrep",
            reason="has intentionally unparsable code",
        ),
        "https://github.com/returntocorp/semgrep-action",
        "https://github.com/returntocorp/semgrep-rules",
        "https://github.com/coinbase/btcexport",
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
        "https://github.com/keybase/python-triplesec",
        "https://github.com/psycopg/psycopg2",
        "https://github.com/preset-io/flask-jwt-extended",
        "https://github.com/vstinner/pyperf",
        "https://github.com/mysql/mysql-connector-python",
        "https://github.com/Netflix/lemur",
        "https://github.com/OWASP/NodeGoat",
        "https://github.com/zulip/zulip",
        "https://github.com/home-assistant/home-assistant",
        "https://github.com/signalapp/Signal-Desktop",
        xfail_repo("https://github.com/highcharts/highcharts"),
        xfail_repo(
            "https://github.com/lodash/lodash",
            reason="https://github.com/returntocorp/semgrep/issues/580",
        ),
        xfail_repo("https://github.com/opensourceactivismtech/call-power"),
        xfail_repo(
            "https://github.com/apache/incubator-superset",
            reason=(
                "https://github.com/returntocorp/semgrep/issues/581, "
                "https://github.com/returntocorp/semgrep/issues/582"
            ),
        ),
        "https://github.com/mpirnat/lets-be-bad-guys",
        "https://github.com/nVisium/django.nV",
        "https://github.com/we45/Vulnerable-Flask-App",
        "https://github.com/JasonHinds13/hackable",
        "https://github.com/ab-smith/gruyere",
        xfail_repo(
            "https://github.com/bkimminich/juice-shop",
            reason="https://github.com/returntocorp/semgrep/issues/581",
        ),
        "https://github.com/DevSlop/Pixi",
        "https://github.com/0c34/govwa",
        "https://github.com/digininja/vuLnDAP",
    ],
)
def test_semgrep_on_repo(monkeypatch, tmp_path, repo_url):
    TESTS_PATH = Path(__file__).parent.parent
    monkeypatch.setenv("PYTHONPATH", str(TESTS_PATH.parent.resolve()))
    (tmp_path / "rules").symlink_to(Path(TESTS_PATH / "qa" / "rules").resolve())
    monkeypatch.chdir(tmp_path)

    subprocess.check_output(
        ["git", "clone", "--depth=1", repo_url, "repo",]
    )
    languages = {
        "python": "py",
        "go": "go",
        "javascript": "js",
    }
    for language, file_ext in languages.items():
        sentinel_path = Path("repo") / f"sentinel.{file_ext}"
        with sentinel_path.open("w") as sentinel_file:
            if language == "go":
                sentinel_file.write(f"package Foo\nconst x = {SENTINEL_VALUE}")
            else:
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

        if output["errors"] != []:
            pytest.fail(
                f"Running on {repo_url} with lang {language} had errors: "
                + json.dumps(output["errors"], indent=4)
            )

        if len(output["results"]) != 1 or output["results"][0]["path"] != str(
            sentinel_path
        ):
            pytest.fail(
                f"Running on {repo_url} with lang {language} expected to have one results instead found result: "
                + json.dumps(output["results"], indent=4)
            )

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

    if len(output["results"]) != 3 or len(output["errors"]) != 0:
        pytest.fail(
            f"Running on {repo_url} with regex rules. Expect 3 results and no errors but got: "
            + json.dumps(output, indent=4)
        )
