import pytest
from semdep.package_restrictions import semver_matches

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
        # pytest.param("<1.0.1", "0.1.0-beta", True),
        pytest.param("<1.0.1", "0.1.0", True),
        # pytest.param("<1.0.1", "2.0", True),
        pytest.param("==5.0.1", "5.0.1", True),
        pytest.param("==5.0.1", "5.0.2", False),
        pytest.param("==5.0.1", "5.0.1-beta", False),
        pytest.param("==5.0.1", "5.0.0", False),
        pytest.param("==5.0.1", "5", False),
        pytest.param("==5.0.1", "5.0.1-release", False),
    ],
)
def test_matches(expression, candidate, match):
    assert semver_matches(expression, candidate) == match