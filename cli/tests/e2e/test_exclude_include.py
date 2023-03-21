import pytest
from tests.fixtures import RunSemgrep


def idfn(options):
    return "-and-".join(flag.strip("-") for flag in options if flag.startswith("--"))


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "options",
    [
        ["--exclude", "excluded.*"],
        ["--include", "included.*"],
        ["--exclude", "excluded"],
        ["--include", "included"],
        ["--include", "included", "--exclude", "excluded.*"],
        ["--exclude", "excluded", "--include", "included.*"],
        ["--exclude", "excluded.*", "--exclude", "included.*"],
        ["--exclude", "excluded", "--exclude", "included"],
        ["--include", "excluded.*", "--include", "included.*"],
        ["--include", "excluded", "--include", "included"],
        ["--include", "included.vue"],
        ["--include", "included.vue", "--skip-unknown-extensions"],
    ],
    ids=idfn,
)
def test_exclude_include(run_semgrep_in_tmp: RunSemgrep, snapshot, options):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        options=options,
        target_name="exclude_include",
        assert_exit_code=None,
    )
    snapshot.assert_match(stdout, "results.json")
    snapshot.assert_match(stderr, "err.out")
