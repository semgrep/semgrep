import pytest
from tests.fixtures import RunSemgrep


# Test in-rule path filtering: paths.include, paths.exclude.
# The patterns now obey the Semgrepignore/Gitignore spec.
@pytest.mark.kinda_slow
def test_paths(run_semgrep_in_tmp: RunSemgrep, posix_snapshot):
    posix_snapshot.assert_match(
        run_semgrep_in_tmp("rules/paths.yaml", target_name="exclude_include").stdout,
        "results.json",
    )
