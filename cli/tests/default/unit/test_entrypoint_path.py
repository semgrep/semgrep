import os
import stat

import pytest

from semgrep.console_scripts.entrypoint import IS_WINDOWS
from semgrep.console_scripts.entrypoint import path_with_scripts_dir
from semgrep.console_scripts.entrypoint import provides_pysemgrep


def make_scripts_dir(root, name, with_pysemgrep=True):
    directory = root / name
    directory.mkdir()
    if with_pysemgrep:
        script = directory / ("pysemgrep.exe" if IS_WINDOWS else "pysemgrep")
        script.write_text("")
        script.chmod(script.stat().st_mode | stat.S_IXUSR)
    return str(directory)


def lookup(path, name="pysemgrep"):
    """The directory a PATH lookup of name would resolve in, as execvp does it."""
    for entry in path.split(os.pathsep):
        if entry and provides_pysemgrep(entry):
            return entry
    return None


@pytest.mark.quick
def test_own_pysemgrep_wins_over_an_install_earlier_in_path(tmp_path):
    """An install invoked by absolute path uses its own pysemgrep.

    Regression test: the scripts directory used to be appended, so a different
    Semgrep sitting earlier in PATH answered the lookup and the install the user
    named ran the other install's pysemgrep.
    """
    ours = make_scripts_dir(tmp_path, "new")
    theirs = make_scripts_dir(tmp_path, "old")

    path = path_with_scripts_dir(os.pathsep.join([theirs, "/usr/bin"]), ours)

    assert lookup(path) == ours


@pytest.mark.quick
def test_ordering_is_untouched_when_nothing_else_provides_pysemgrep(tmp_path):
    """The common case must not reorder PATH.

    semgrep shells out to `git` by name, so moving the scripts directory --
    which is a shared bin directory for Homebrew, a system Python, or the Docker
    image -- ahead of the rest of PATH would change which one runs.
    """
    ours = make_scripts_dir(tmp_path, "scripts")
    original = os.pathsep.join(["/opt/tools/bin", "/usr/local/bin", "/usr/bin"])

    path = path_with_scripts_dir(original, ours)

    assert path == os.pathsep.join(
        ["/opt/tools/bin", "/usr/local/bin", "/usr/bin", ours]
    )


@pytest.mark.quick
def test_scripts_dir_already_on_path_is_not_moved_ahead_of_it(tmp_path):
    ours = make_scripts_dir(tmp_path, "scripts")
    original = os.pathsep.join(["/opt/tools/bin", ours, "/usr/bin"])

    path = path_with_scripts_dir(original, ours)

    assert path.split(os.pathsep)[0] == "/opt/tools/bin"
    assert lookup(path) == ours


@pytest.mark.quick
def test_scripts_dir_without_a_pysemgrep_is_only_appended(tmp_path):
    """Nothing of ours to protect, so the ordering is left alone."""
    ours = make_scripts_dir(tmp_path, "scripts", with_pysemgrep=False)
    theirs = make_scripts_dir(tmp_path, "other")

    path = path_with_scripts_dir(theirs, ours)

    assert path == os.pathsep.join([theirs, ours])


@pytest.mark.quick
def test_empty_path(tmp_path):
    ours = make_scripts_dir(tmp_path, "scripts")

    assert path_with_scripts_dir("", ours) == ours


@pytest.mark.quick
@pytest.mark.skipif(IS_WINDOWS, reason="POSIX permission bits")
def test_a_non_executable_file_does_not_answer_the_lookup(tmp_path):
    directory = tmp_path / "not-a-script"
    directory.mkdir()
    (directory / "pysemgrep").write_text("")

    assert not provides_pysemgrep(str(directory))
