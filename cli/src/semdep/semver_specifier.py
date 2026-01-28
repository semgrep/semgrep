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
"""
Version range matching using SemVer as the version syntax
"""
from typing import Any
from typing import Optional

import semantic_version as sv  # type: ignore[import-not-found]

# Sadly, semantic-version doesn't come with mypy types.


# Docs: https://python-semanticversion.readthedocs.io/en/latest/
def _parse_specifier(s: str) -> Any:
    # Semgrep rules use spaces after commas but this lib doesn't tolerate them
    expr = s.replace(" ", "")
    try:
        return sv.SimpleSpec(expr)
    except ValueError as e:
        raise ValueError(f"Invalid semver range: {expr}") from e


def _parse_version(s: str) -> Any:
    try:
        return sv.Version(s)
    except ValueError:
        # fail if not strictly SemVer-compliant so as to allow a fallback
        # implementation
        return None


def _version_matches(spec: Any, version: Any) -> bool:
    res: bool = spec.match(version)
    return res


def version_matches_str(spec: str, version_str: str) -> Optional[bool]:
    """Parse a version specifier and a version and check if they match.

    For faster results, consider preparsing one or both arguments and using
    '_version_matches' above.
    """
    try:
        version = _parse_version(version_str)
        if version is not None:
            return _version_matches(_parse_specifier(spec), version)
        else:
            return None
    except ValueError:
        return None
