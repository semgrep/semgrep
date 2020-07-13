import pipes
import subprocess
from pathlib import Path
from typing import NamedTuple

import pytest

from ..conftest import TESTS_PATH

EXTENSIONS = {
    "python": "py",
    "go": "go",
    "javascript": "js",
}


def sloc(path, extension=None):
    """Quick hack in lieu of installing sloccount"""
    if extension:
        extension_pat = f"-name '*.{extension}'"
    else:
        extension_pat = ""
    t = pipes.Template()
    t.prepend(f"find {path} {extension_pat}", ".-")
    t.append("xargs cat", "--")
    t.append("wc -l", "--")
    f = t.open("pipefile", "r")
    return int(f.read())


def test_sloc():
    assert sloc(path=".", extension="py") > 0


class RepoCase(NamedTuple):
    url: str
    sha: str
    language: str
    # used to decouple test runs when we change what the
    # test is actually doing for benchmarking
    version: str = "1"

    @property
    def test_id(self):
        repo_name = self.url.split("/")[-1]
        return f"{repo_name}-v{self.version}"


@pytest.mark.parametrize(
    "repo_case",
    [
        # SHAs of May 6, 2020
        RepoCase(
            "https://github.com/coinbase/node-process-lock", "dd687bc", "javascript"
        ),
        RepoCase("https://github.com/getsentry/sentry", "f25ea5dc", "python"),
        RepoCase(
            "https://github.com/bkimminich/juice-shop.git", "e03629b", "javascript"
        ),
    ],
    ids=lambda case: case.test_id,
)
def test_default_packs(run_semgrep_in_tmp, clone_github_repo, benchmark, repo_case):
    """
    This test _intentionally_ depends on the composition of the packs. If we add a slow check to a pack, we want to know
    about it!
    """
    repo_path = clone_github_repo(repo_url=repo_case.url, sha=repo_case.sha)
    repo_ksloc = sloc(str(repo_path), extension=EXTENSIONS[repo_case.language]) / 1000

    # In general, aim for 1ksloc / rule. The packs are "special" though -- composed of rules we know will run
    # fast and give great UX
    DEFAULT_PACK_MIN_SPEED_KSLOCS = 0.1
    timeout = max(repo_ksloc / DEFAULT_PACK_MIN_SPEED_KSLOCS, 20)
    print(
        f"checking with timeout of {timeout} (required analysis speed of {repo_ksloc / timeout} kslocs"
    )

    def run_benchmark():
        try:
            subprocess.check_output(
                [
                    "python3",
                    "-m",
                    "semgrep",
                    "--jobs",
                    "1",
                    "--config",
                    f"https://semgrep.live/c/p/{repo_case.language}",
                    str(repo_path),
                ],
                timeout=timeout,
            )
        except subprocess.TimeoutExpired:
            # Avoid a giant and useless pytest stack trace
            pytest.fail(f"check-pack benchmark timed out (running {repo_case})")

    benchmark(run_benchmark)


@pytest.mark.parametrize(
    "repo_case",
    [
        RepoCase(
            "https://github.com/draios/sysdig-inspect", "63f17b9", language="javascript"
        ),
        RepoCase("https://github.com/getsentry/sentry", "f25ea5dc", language="python"),
        RepoCase(
            "https://github.com/signalapp/signal-webrtc-ios",
            "6edf8c3",
            language="python",
        ),
        RepoCase("https://github.com/secdev/scapy", "41862d7", language="python"),
        RepoCase(
            "https://github.com/coinbase/rosetta-sdk-go", "26ae7bd", language="go"
        ),
        # NOTE: this repo is mostly typescript
        RepoCase(
            "https://github.com/coinbase/rest-hooks", "d5cf9c7", language="javascript",
        )
        # "https://github.com/getsentry/sentry-python",
        # "https://github.com/apache/airflow",
        # "https://github.com/preset-io/elasticsearch-dbapi",
        # "https://github.com/apache/libcloud",
        # "https://github.com/keybase/pykeybasebot",
        # "https://gitbox.apache.org/repos/asf/cassandra",
        # "https://github.com/coinbase/coinbase-commerce-python",
        # "https://github.com/keybase/python-triplesec",
        # "https://github.com/psycopg/psycopg2",
        # "https://github.com/preset-io/flask-jwt-extended",
        # "https://github.com/vstinner/pyperf",
        # "https://github.com/mysql/mysql-connector-python",
        # "https://github.com/Netflix/lemur",
    ],
    ids=lambda case: case.test_id,
)
@pytest.mark.parametrize(
    "language", ["python", "java", "go", "javascript"], ids=lambda lang: lang
)
def test_trivial_pattern(
    run_semgrep_in_tmp, clone_github_repo, benchmark, repo_case, language
):
    """
    This test runs a trivial pattern over the repo, essentially tracking our performance of parsing the language code.
    We run every repo for all the primary languages. This allows us to also track off-language results (how fast do
    we detect that we don't need to scan this repo)
    """
    repo_path = clone_github_repo(repo_url=repo_case.url, sha=repo_case.sha)
    print(repo_path)
    benchmark(
        subprocess.check_output,
        [
            "python3",
            "-m",
            "semgrep",
            "--jobs",
            "1",
            "--pattern",
            "$X = 156128578192758",
            "--lang",
            language,
            repo_path,
        ],
    )


class PerformanceBug(NamedTuple):
    rule: Path
    file: Path


@pytest.mark.parametrize(
    "perf_bug",
    [
        PerformanceBug(
            rule=Path("rules/request-data-write.yaml"),
            file=Path("targets/definitions.py"),
        )
    ],
    ids=lambda lang: lang,
)
def test_performance_bugs(benchmark, perf_bug):
    """
    Benchmark for file/rule combinations that have been historically slow to watch for regressions.
    """
    benchmark.extra_info = {"ksloc": sloc(TESTS_PATH / "performance" / perf_bug.file)}
    benchmark(
        subprocess.check_output,
        [
            "python3",
            "-m",
            "semgrep",
            "--jobs",
            "1",
            "--config",
            TESTS_PATH / "performance" / perf_bug.rule,
            TESTS_PATH / "performance" / perf_bug.file,
        ],
    )


def test_version(benchmark):
    benchmark(subprocess.check_output, ["python3", "-m", "semgrep", "--version"])
