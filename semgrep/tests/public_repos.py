import pytest


def xfail_repo(repo, *, reason=""):
    return pytest.param(repo, marks=pytest.mark.xfail(reason=reason, strict=True))


ALL_LANGUAGES = ["ALL"]


PASSING_REPOS = [
    {"repo": "https://github.com/coinbase/bifrost", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/bip38", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/btcexport", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/coinbase/coinbase-android-sdk",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/coinbase/coinbase-android-sdk-example",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/coinbase/coinbase-bitmonet-sdk",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/coinbase/coinbase-commerce-node",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/coinbase/coinbase-commerce-python",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/coinbase/coinbase-exchange-node",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/coinbase/coinbase-java", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/coinbase/coinbase-javascript-sdk",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/coinbase/coinbase-node", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/coinbase-python", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/coinbase/coinbase-tip-discourse",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/coinbase/dexter", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/fenrir", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/coinbase/node-process-lock",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/coinbase/odin", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/protoc-gen-rbi", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/pwnbot", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/rosetta-cli", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/rosetta-sdk-go", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/coinbase/self-service-iam",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/coinbase/solidity-workshop",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/coinbase/step", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/coinbase/step-asg-deployer",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/coinbase/watchdog", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/AskBox", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/Developer-Samples",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/dropbox/changes-artifacts",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/changes-client", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/changes-lxc-wrapper",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/dropbox/changes-mesos-framework",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/dropbox/dbx-unittest2pytest",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/dbxcli", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/dropbox-api-content-hasher",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/dropbox/dropbox-api-v2-repl",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/dropbox/dropbox-sdk-go-unofficial",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/dropbox-sdk-java", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/dropbox-sdk-js", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/dropbox-sdk-python",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/dropbox_hook", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/emmer", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/firebase-dropbox-oauth",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/git-rbr", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/PyHive", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/goebpf", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/goprotoc", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/grallama-panel", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/groupy", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/hermes", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/hocrux", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/hydra", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/llama", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/llama-archive", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/load_management", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/mdwebhook", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/merou", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/mypy-PyCharm-plugin",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/mypy-protobuf", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/nautilus-dropbox", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/nsot", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/othw", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/pem-converter-maven-plugin",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/pep8squad", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/presto-kafka-connector",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/puppet_run", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/pyannotate", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/pygerduty", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/pynsot", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/dropbox/pytest-call-tracer",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/dropbox/pytest-flakefinder",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/dropbox/python-invariant", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/revision-browser", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/rules_node", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/securitybot", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/spookify", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/sqlalchemy-stubs", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/stone", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/stopwatch", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/strongpoc", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/whitegold", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/ykfipsconf", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/zinger", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/returntocorp/badwords", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/returntocorp/bento-report",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/returntocorp/buffer-rule-tests",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/returntocorp/check-docs", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/returntocorp/cli", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/returntocorp/flake8-click",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/returntocorp/flake8-flask",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/returntocorp/flake8-requests",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/returntocorp/inputset-generator",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/returntocorp/semgrep-action",
        "languages": ALL_LANGUAGES,
    },
    {
        "repo": "https://github.com/returntocorp/semgrep-rules",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/coinbase/btcexport", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/seemoo-lab/opendrop", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/lightstep/lightstep-tracer-python",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/draios/sysdig-inspect", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/getsentry/sentry-python", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/signalapp/signal-webrtc-ios",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/secdev/scapy", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/apache/airflow", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/preset-io/elasticsearch-dbapi",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/apache/libcloud", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/keybase/pykeybasebot", "languages": ALL_LANGUAGES},
    {
        "repo": "https://gitbox.apache.org/repos/asf/cassandra",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/keybase/python-triplesec", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/psycopg/psycopg2", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/preset-io/flask-jwt-extended",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/vstinner/pyperf", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/mysql/mysql-connector-python",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/Netflix/lemur", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/mpirnat/lets-be-bad-guys", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/JasonHinds13/hackable", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/ab-smith/gruyere", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/digininja/vuLnDAP", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/godropbox", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/dropbox/trapperkeeper", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/lodash/lodash", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/zulip/zulip", "languages": ALL_LANGUAGES},
    {
        "repo": "https://github.com/home-assistant/home-assistant",
        "languages": ALL_LANGUAGES,
    },
    {"repo": "https://github.com/0c34/govwa", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/bittip", "languages": ALL_LANGUAGES},
    {"repo": "https://github.com/coinbase/multisig-tool", "languages": ALL_LANGUAGES,},
    {
        "repo": "https://github.com/we45/Vulnerable-Flask-App",
        "languages": ALL_LANGUAGES,
    },
]

FAILING_REPOS = [
    xfail_repo(
        {"repo": "https://github.com/jekyll/jekyll", "languages": ["ruby"],},
        reason="ruby parse error on '!old_scope.key? \"type\"'",
    ),
    xfail_repo(
        {
            "repo": "https://github.com/highcharts/highcharts",
            "languages": ALL_LANGUAGES,
        },
        reason="javascript parse error, probably due to '?.01' regression in minified file",
    ),
    xfail_repo(
        {"repo": "https://github.com/OWASP/NodeGoat", "languages": ALL_LANGUAGES},
        reason="javascript parse error, probably due to 's.behaveLikeLine?.8:1' regression in minified file",
    ),
    xfail_repo(
        {"repo": "https://github.com/coinbase/gtt-ui", "languages": ALL_LANGUAGES},
        reason="javascript parse error, probably due to ?. regression",
    ),
    xfail_repo(
        {"repo": "https://github.com/dropbox/questions", "languages": ALL_LANGUAGES},
        reason="javascript parse error, probably due to ?. regression",
    ),
    xfail_repo(
        {"repo": "https://github.com/DevSlop/Pixi", "languages": ALL_LANGUAGES},
        reason="javascript parse error, probably due to ?. regression",
    ),
    xfail_repo(
        {
            "repo": "https://github.com/dropbox/incubator-superset-internal",
            "languages": ALL_LANGUAGES,
        },
        reason="javascript parse error",
    ),
    xfail_repo(
        {
            "repo": "https://github.com/coinbase/react-coinbase-commerce",
            "languages": ALL_LANGUAGES,
        },
        reason="react",
    ),
    xfail_repo(
        {
            "repo": "https://github.com/dropbox/DropboxBusinessScripts",
            "languages": ALL_LANGUAGES,
        },
        reason="https://github.com/returntocorp/semgrep/issues/1498",
    ),
    xfail_repo(
        {
            "repo": "https://github.com/dropbox/dbx_build_tools",
            "languages": ALL_LANGUAGES,
        },
        reason="go parse errors",
    ),
    xfail_repo(
        {"repo": "https://github.com/dropbox/hackpad", "languages": ALL_LANGUAGES},
        reason="https://www.oreilly.com/library/view/javascript-the-definitive/9781449393854/ch11s06.html",
    ),
    xfail_repo(
        {"repo": "https://github.com/dropbox/notouch", "languages": ALL_LANGUAGES},
        reason="indented comment on last line python",
    ),
    xfail_repo({"repo": "https://github.com/rails/rails", "languages": ["ruby"]}),
    xfail_repo(
        {
            "repo": "https://github.com/rapid7/metasploit-framework",
            "languages": ["ruby"],
        }
    ),
    xfail_repo({"repo": "https://github.com/Homebrew/brew", "languages": ["ruby"]}),
    xfail_repo({"repo": "https://github.com/gitlabhq/gitlabhq", "languages": ["ruby"]}),
    xfail_repo(
        {
            "repo": "https://github.com/coinbase/coinbase-pro-node",
            "languages": ["javascript", "typescript"],
        },
        reason=("Failure to parse typescript"),
    ),
    xfail_repo(
        {
            "repo": "https://github.com/bkimminich/juice-shop",
            "languages": ["javascript", "typescript"],
        },
        reason=("Failure to parse typescript"),
    ),
    xfail_repo(
        {"repo": "https://github.com/dropbox/changes", "languages": ALL_LANGUAGES},
        reason="react",
    ),
    xfail_repo(
        {"repo": "https://github.com/dropbox/pyston-perf", "languages": ALL_LANGUAGES},
        reason="templates",
    ),
    xfail_repo(
        {
            "repo": "https://github.com/opensourceactivismtech/call-power",
            "languages": ALL_LANGUAGES,
        },
        reason="templates",
    ),
]

# Access this list with the `public_repo_url` fixture argument.
ALL_REPOS = FAILING_REPOS + PASSING_REPOS
