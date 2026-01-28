#
# Copyright (c) 2023-2024 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import pytest

from semdep.package_restrictions import is_in_range
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi

# TODO: test_maven_matches


# Version matching is ecosystem-dependent. Modern systems might settle on
# SemVer but many support their own scheme often predating SemVer (2011).
#
# Currently, our Python code uses PEP 440 by default which is
# the standard used by PyPi (Python packages). This does not work
# fully correctly in general.
#
# NPM uses SemVer (MAJOR.MINOR.PATCH-optprerelease+optbuild).
# PyPi uses PEP 440 (supports things like 1.2 or 1.2.3.4 and different
# ordering than SemVer when it comes to the -suffix).
# Go supports something called pseudoversions.
# Maven is wild.
#
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
        pytest.param("<2.0.0", "2.0.0", False),
        pytest.param("<1.0.1", "2.0", False, marks=pytest.mark.xfail),
        pytest.param("==5.0.1", "5.0.1", True),
        pytest.param("==5.0.1", "5.0.2", False),
        pytest.param("==5.0.1", "5.0.1-beta", False),
        pytest.param("==5.0.1", "5.0.0", False),
        pytest.param("==5.0.1", "5", False),
        pytest.param("==5.0.1", "5.0.1-release", False),
        # This works differently in NPM
        pytest.param("==2.0.0", "2.0.0-beta", False),
        pytest.param(">=2.0.0", "2.0.0-beta", False),
        pytest.param("<=2.0.0", "2.0.0-beta", False),
        pytest.param("<2.0.0", "2.0.0-beta", False),
        pytest.param(">2.0.0", "2.0.0-beta", False),
        # Exact version matching regardless of syntax
        pytest.param("==some-string", "some-string", True),
        # Non-SemVer N versions
        pytest.param(">2", "2", False),
        pytest.param("<2", "2", False),
        pytest.param("==2", "2", True),
        pytest.param(">=2", "2", True),
        pytest.param("<=2", "2", True),
        # Non-SemVer A.B versions
        pytest.param(">2.0", "2.0", False),
        pytest.param("<2.0", "2.0", False),
        pytest.param("==2.0", "2.0", True),
        pytest.param(">=2.0", "2.0", True),
        pytest.param("<=2.0", "2.0", True),
        # Non-SemVer A.B.C.D versions
        pytest.param(">1.2.3.4", "1.2.3.4", False),
        pytest.param("<1.2.3.4", "1.2.3.4", False),
        pytest.param("==1.2.3.4", "1.2.3.4", True),
        pytest.param(">=1.2.3.4", "1.2.3.4", True),
        pytest.param("<=1.2.3.4", "1.2.3.4", True),
        # Spacing
        pytest.param("< 2.0.0", "2.0.0", False),
        pytest.param("== 2.0.0", "2.0.0", True),
        # Intervals
        pytest.param(">= 1.0.0, < 2.0.0", "1.0.0", True),
        pytest.param(">= 1.0.0, < 2.0.0", "1.2.0", True),
        pytest.param(">= 1.0.0, < 2.0.0", "2.0.0", False),
        # Flipped bounds
        pytest.param("< 2.0.0, >= 1.0.0", "1.0.0", True),
        #
        # TODO: more tests would be great,
        #  based on what we find in sca-rules/rules/
        #  Don't hesitate to add some xfails (cases to be implemented later).
        #
    ],
)
def test_generic_matches(expression, candidate, match):
    assert is_in_range(Ecosystem(Pypi()), expression, candidate) == match


# NPM uses SemVer for version syntax + nontrivial rules for version matching
# which may not be the same in other ecosystems using SemVer (need to check).
#
# Note that we only support basic operators: == < > <= >=
# as published by GHSA and used in sca-rules.
#
@pytest.mark.quick
@pytest.mark.parametrize(
    "expression,candidate,match",
    [
        (">1.0.0", "1.0.1", True),
        (">1.0.0", "2.0.0", True),
        (">=1.0.0", "1.0.0", True),
        (">1.0.0", "0.1.0", False),
        (">1.0.0", "0.1.0-beta", False),
        ("<1.0.1", "1.0.1", False),
        ("<1.0.1", "1.0.0", True),
        ("<1.0.1", "0.1.0-beta", True),
        ("<1.0.1", "0.1.0", True),
        ("<2.0.0", "2.0.0", False),
        ("<1.0.1", "2.0", False),
        ("==5.0.1", "5.0.1", True),
        ("==5.0.1", "5.0.2", False),
        ("==5.0.1", "5.0.1-beta", False),
        ("==5.0.1", "5.0.0", False),
        ("==5.0.1", "5", False),
        ("==5.0.1", "5.0.1-release", False),
        # This block highlights the differences between NPM version matching
        # and ordinary math operations
        ("==2.0.0", "2.0.0-beta", False),
        (">=2.0.0", "2.0.0-beta", False),
        ("<=2.0.0", "2.0.0-beta", True),
        ("<2.0.0", "2.0.0-beta", False),  # not your usual math
        (">2.0.0", "2.0.0-beta", False),
        # Check that spaces are ignored
        (">= 1.0.0", "1.2.3", True),
        (">= 2.0.0", "1.2.3", False),
        # Check intervals
        ("<= 2.3.4, >= 2.3.4-a", "2.3.4-b", True),
        # Check prereleases as range boundaries
        (">= 0.20.0-alpha", "0.20.0-alpha", True),
        ("== 0.20.0-alpha", "0.20.0-alpha", True),
        ("> 0.20.0-alpha", "0.20.0-alpha", False),
        # Always fail with non-SemVer versions
        (">= 1.2.3", "1.2.3.0", False),
        ("== 1.3.3", "1.3.3.4", False),
        ("== 1.2.0", "1.2", False),
        (">= 1.2.0-alpha.0", "1.2-alpha.3", False),
        (">= 1.2.0-alpha.0, <= 1.2.0", "1.2-alpha.3", False),
        ("== 1.2.0", "1.2alpha.3", False),
        ("==some-string", "some-string", False),
    ],
)
def test_npm_matches(expression, candidate, match):
    assert is_in_range(Ecosystem(Npm()), expression, candidate) == match
