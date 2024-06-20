import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat

# TODO: merge with test_ssc.py?
# was in e2e/test_output.py before


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sca_output(run_semgrep_on_copied_files: RunSemgrep, snapshot):
    results, _errors = run_semgrep_on_copied_files(
        "rules/dependency_aware/monorepo_with_first_party.yaml",
        target_name="dependency_aware/monorepo",
        output_format=OutputFormat.TEXT,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )


@pytest.mark.kinda_slow
@pytest.mark.osemfail
def test_sca_lockfile_only_output(run_semgrep_on_copied_files: RunSemgrep, snapshot):
    results, _errors = run_semgrep_on_copied_files(
        "rules/dependency_aware/lodash-4.17.19.yaml",
        target_name="dependency_aware/unreachable_multiple_copies/yarn.lock",
        output_format=OutputFormat.TEXT,
    )
    snapshot.assert_match(
        results,
        "results.txt",
    )
