import pytest
from tests.fixtures import RunSemgrep


@pytest.mark.osempass
@pytest.mark.kinda_slow
def test_extract(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Validates that Semgrep works with extract mode
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/extract_rules/js_html_concat.yaml",
            target_name="extract/js_html_concat.html",
        ).stdout,
        "results.json",
    )


@pytest.mark.osempass
@pytest.mark.kinda_slow
def test_extract_exclude(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Validates that Semgrep works with extract mode
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/extract_rules/python_jupyter_paths_exclude.yaml",
            target_name="extract/python_jupyter_paths_exclude.ipynb",
        ).stdout,
        "results.json",
    )


@pytest.mark.osempass
@pytest.mark.kinda_slow
def test_extract_include(run_semgrep_in_tmp: RunSemgrep, snapshot):
    """
    Validates that Semgrep works with extract mode
    """
    snapshot.assert_match(
        run_semgrep_in_tmp(
            "rules/extract_rules/python_jupyter_paths_include.yaml",
            target_name="extract/python_jupyter_paths_include.ipynb",
        ).stdout,
        "results.json",
    )
