#
# Copyright (c) 2026 Semgrep Inc.
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
# Rule package names are matched against dependency names after PEP 503
# normalization for PyPI: rules and lockfile parsers don't all spell names the
# same way (e.g. "zope.interface" in a rule, "zope-interface" in poetry.lock).
from collections import defaultdict

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semdep.package_restrictions import dependencies_range_match_any
from semgrep.dependency_aware_rule import dependency_match_key
from semgrep.dependency_aware_rule import SubprojectDependencyIndex
from semgrep.subproject import from_resolved_dependencies

PYPI = out.Ecosystem(out.Pypi())
NPM = out.Ecosystem(out.Npm())


def _dep(package, version, ecosystem=PYPI):
    return out.ResolvedDependency(
        (
            out.FoundDependency(
                package=package,
                version=version,
                ecosystem=ecosystem,
                allowed_hashes=defaultdict(list),
                transitivity=out.DependencyKind(out.Direct()),
                resolved_url=None,
                git_ref=None,
                lockfile_path=out.Fpath("poetry.lock"),
                line_number=1,
            ),
            None,
        )
    )


def _index(deps, ecosystem=PYPI):
    source = out.DependencySource(
        out.LockfileOnly(
            out.Lockfile(out.LockfileKind(out.PoetryLock()), out.Fpath("poetry.lock"))
        )
    )
    subproject = out.ResolvedSubproject(
        info=out.Subproject(
            root_dir=out.Fpath("."),
            dependency_source=source,
            ecosystem=ecosystem,
        ),
        resolution_method=out.ResolutionMethod(out.LockfileParsing()),
        ecosystem=ecosystem,
        resolved_dependencies=from_resolved_dependencies(deps),
        errors=[],
    )
    return SubprojectDependencyIndex.from_subproject(subproject)


def _matches(index, package, semver_range, ecosystem=PYPI):
    pattern = out.ScaPattern(
        ecosystem=ecosystem, package=package, semver_range=semver_range
    )
    return [
        (p.package, d.package, d.version)
        for p, d in index.get_dependency_matches([pattern])
    ]


@pytest.mark.quick
@pytest.mark.parametrize(
    "ecosystem,package,expected",
    [
        (PYPI, "zope.interface", "zope-interface"),
        (PYPI, "Typing_Extensions", "typing-extensions"),
        (PYPI, "changedetection.io", "changedetection-io"),
        (PYPI, "a__b.-c", "a-b-c"),
        (NPM, "@Scope/Pkg.js", "@Scope/Pkg.js"),
    ],
)
def test_dependency_match_key(ecosystem, package, expected):
    assert dependency_match_key(ecosystem, package) == expected


@pytest.mark.quick
def test_dotted_rule_name_matches_normalized_dependency():
    index = _index([_dep("zope-interface", "5.4.0")])
    assert _matches(index, "zope.interface", "<6.0.0") == [
        ("zope.interface", "zope-interface", "5.4.0")
    ]


@pytest.mark.quick
def test_normalized_rule_name_matches_dotted_dependency():
    index = _index([_dep("zope.event", "4.5.0")])
    assert _matches(index, "zope-event", "<5.0.0") == [
        ("zope-event", "zope.event", "4.5.0")
    ]


@pytest.mark.quick
def test_underscore_and_case_differences_match():
    index = _index([_dep("typing-extensions", "4.0.0")])
    assert len(_matches(index, "typing_extensions", "<4.5.0")) == 1


@pytest.mark.quick
def test_version_range_still_applies():
    index = _index([_dep("zope-interface", "7.1.0")])
    assert _matches(index, "zope.interface", "<6.0.0") == []


@pytest.mark.quick
def test_other_ecosystems_match_exactly():
    index = _index([_dep("lodash.merge", "4.6.0", NPM)], NPM)
    assert _matches(index, "lodash-merge", "<4.6.2", NPM) == []
    assert len(_matches(index, "lodash.merge", "<4.6.2", NPM)) == 1


@pytest.mark.quick
def test_rule_prefilter_matches_across_spellings():
    # filter_dependency_aware_rules and reachable findings use this function:
    # a rule for "zope.interface" must not be dropped for "zope-interface"
    dep = _dep("zope-interface", "5.4.0").value[0]
    pattern = out.ScaPattern(
        ecosystem=PYPI, package="zope.interface", semver_range="<6.0.0"
    )
    assert list(dependencies_range_match_any([pattern], [dep])) == [(pattern, dep)]
