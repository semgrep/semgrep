import pytest
from tests.fixtures import RunSemgrep

from semgrep.constants import OutputFormat


@pytest.mark.skip(reason="TODO: make osemgrep output match pysemgrep")
@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "output_format",
    [OutputFormat.SARIF],
)
def test_sarif_output(run_semgrep_in_tmp: RunSemgrep, output_format):
    _out, err = run_semgrep_in_tmp(
        "rules/eqeq.yaml",
        target_name="basic/stupid.py",
        options=["--verbose", "--use-osemgrep-format-output"],
        output_format=output_format,
        assert_exit_code=0,
    )
    # When --verbose is enabled, we log it when osemgrep and pysemgrep
    # outputs don't match. See logger.verbose() example in osemgrep_sarif.py.
    assert "output mismatch" not in err
