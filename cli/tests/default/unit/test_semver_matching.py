import pytest

from semdep.package_restrictions import is_in_range
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm


@pytest.mark.quick
@pytest.mark.parametrize(
    "expression,candidate,match",
    [
        pytest.param(">1.0.0", "1.0.1", True),
        pytest.param(">1.0.0", "2.0.0", True),
        pytest.param(">=1.0.0", "1.0.0", True),
        pytest.param(">1.0.0", "0.1.0", False),
        pytest.param(">1.0.0", "0.1.0-beta", False),
        pytest.param("<1.0.1", "1.0.1", False),
        pytest.param("<1.0.1", "1.0.0", True),
        pytest.param("<1.0.1", "0.1.0-beta", True, marks=pytest.mark.xfail),
        pytest.param("<1.0.1", "0.1.0", True),
        pytest.param("<1.0.1", "2.0", True, marks=pytest.mark.xfail),
        pytest.param("==5.0.1", "5.0.1", True),
        pytest.param("==5.0.1", "5.0.2", False),
        pytest.param("==5.0.1", "5.0.1-beta", False),
        pytest.param("==5.0.1", "5.0.0", False),
        pytest.param("==5.0.1", "5", False),
        pytest.param("==5.0.1", "5.0.1-release", False),
        pytest.param("==some-string", "some-string", True),
    ],
)
def test_matches(expression, candidate, match):
    # Can be any ecosystem except Maven, which has custom version operation
    assert is_in_range(Ecosystem(Npm()), expression, candidate) == match
