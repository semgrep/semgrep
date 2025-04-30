import pytest
from tests.fixtures import RunSemgrep


def _name_of_flag(flag: str) -> str:
    """
    Extract just the name of a CLI flag. E.g.,

    ```
    assert _name_of_flag("--include") == "include"
    assert _name_of_flag("--include=SomeArgument") == "include"
    assert _name_of_flag("--include=SomeArgument") == "include"
    assert _name_of_flag("--x-ls") == "x-ls"
    ```
    """
    without_dashes = flag.strip("-")
    # in case the flag has an argument passed with "=", remove the argument
    name, *_ = without_dashes.split("=")
    return name


def idfn(options):
    """
    Identify a test by joining the names of the flags it takes as a parameter
    with `"-and-"`. E.g.,

    - If the test takes the options `["--x-ls", "--exclude", "foo" ]`, it
      will be identified with the prameter ID "x-ls-and-exclude".
    - If the test takes the options `["--x-ls", "--include=bar" ]`, it
      will be identified with the prameter ID "x-ls-and-include".

    Note that both arguments to flags and their `--` prefixes are removed.
    ```
    """
    return "-and-".join(
        _name_of_flag(flag) for flag in options if flag.startswith("--")
    )


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
        # On Windows, the '*.*' argument is expanded as a wildcard by the OCaml
        # runtime. This breaks the test since the files in the current
        # directory are wrongly treated as scanning roots. As a workaround, we
        # pass the *.* value using --include=
        LS + ["--exclude=*.*"],
        LS + ["--include=*.*"],
    ],
    ids=idfn,
)
def test_exclude_include_file_list(
    run_semgrep_in_test_folder: RunSemgrep, posix_snapshot, options
):
    stdout, stderr = run_semgrep_in_test_folder(
        "rules/eqeq.yaml",  # unused
        # adding the new common options here to avoid renaming the
        # snapshot files
        options=options,
        target_name="exclude_include",
        osemgrep_force_project_root=".",
        assert_exit_code=None,
    )
    posix_snapshot.assert_match(stdout, "files.list")
