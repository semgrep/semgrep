import re

import pytest
from tests.fixtures import RunSemgrep


def idfn(options):
    return "-and-".join(flag.strip("-") for flag in options if flag.startswith("--"))


IGNORED_TARGET_LINE = re.compile("ignored .*\n")


# We mask the lines such as the following in the output of --x-ls:
# ignored targets/exclude_include/excluded/excluded.js [cli_exclude_flags_match]
# because it's only a debug help and osemgrep and pysemgrep show different
# things.
def mask_ignored(text: str) -> str:
    return re.sub(IGNORED_TARGET_LINE, "", text)


# --quiet is to suppress logs that differ between pysemgrep and osemgrep.
LS = ["--x-ls"]


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "options",
    [
        LS + ["--exclude", "excluded.*"],
        LS + ["--include", "included.*"],
        LS + ["--exclude", "excluded"],
        LS + ["--include", "included"],
        LS + ["--include", "included", "--exclude", "excluded.*"],
        LS + ["--exclude", "excluded", "--include", "included.*"],
        LS + ["--exclude", "excluded.*", "--exclude", "included.*"],
        LS + ["--exclude", "excluded", "--exclude", "included"],
        LS + ["--include", "excluded.*", "--include", "included.*"],
        LS + ["--include", "excluded", "--include", "included"],
        LS + ["--include", "included.vue"],
        LS + ["--include", "included.vue", "--skip-unknown-extensions"],
        LS + ["--exclude", "*.*"],
        LS + ["--include", "*.*"],
    ],
    ids=idfn,
)
@pytest.mark.osemfail
def test_exclude_include(run_semgrep_in_tmp: RunSemgrep, snapshot, options):
    stdout, stderr = run_semgrep_in_tmp(
        "rules/eqeq.yaml",  # unused
        options=options,
        target_name="exclude_include",
        assert_exit_code=None,
    )
    snapshot.assert_match(mask_ignored(stdout), "files.list")
