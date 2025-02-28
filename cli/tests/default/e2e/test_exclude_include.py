import pytest
from tests.fixtures import RunSemgrep


def idfn(options):
    return "-and-".join(flag.strip("-") for flag in options if flag.startswith("--"))


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
def test_exclude_include(run_semgrep_in_test_folder: RunSemgrep, snapshot, options):
    stdout, stderr = run_semgrep_in_test_folder(
        "rules/eqeq.yaml",  # unused
        # adding the new common options here to avoid renaming the
        # snapshot files
        options=options,
        target_name="exclude_include",
        osemgrep_force_project_root=".",
        assert_exit_code=None,
    )
    snapshot.assert_match(stdout, "files.list")
