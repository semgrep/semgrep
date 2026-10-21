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
"""Check that system TLS trust sources exclude the eMudhra incident roots."""
import base64
import hashlib
import os
import re
from pathlib import Path


# Source: https://github.com/galadran/we-have-a-problem.com/blob/main/roots.json
BLOCKED_ROOTS = {
    "7dd78d5f4f13459a83dff9abbba62edbaf6f2d102bf257fd712f4d9f2746ed8d": (
        "emSign Root TLS CA - G3"
    ),
    "f2edaaff3d9b701fb4d14c9f285f8c0bad90fc3bcc1fa07d47f4718edaba34a5": (
        "emSign Root CS CA - G3"
    ),
    "d7a7a0fb5d7e2731d771e9484ebcdef71d5f0c3e0a2948782bc83ee0ea699ef4": (
        "AAA Certificate Services"
    ),
    "86a1ecba089c4a8d3bbe2734c612ba341d813e043cf9e8a862cd5c57a36bbe6b": (
        "emSign ECC Root CA - G3"
    ),
}
PEM_CERTIFICATE = re.compile(
    rb"-----BEGIN CERTIFICATE-----\s*(.*?)\s*-----END CERTIFICATE-----",
    re.DOTALL,
)
BUNDLES = (
    Path("/etc/ssl/certs/ca-certificates.crt"),
    Path("/etc/ssl/cert.pem"),
)
SOURCES = (
    Path("/etc/ssl/certs"),
    Path("/usr/share/ca-certificates"),
    Path("/usr/local/share/ca-certificates"),
)


def certificate_fingerprints(path: Path) -> set[str]:
    """Return SHA-256 fingerprints for every PEM certificate in a file."""
    try:
        contents = path.read_bytes()
    except OSError:
        return set()

    fingerprints = set()
    for match in PEM_CERTIFICATE.finditer(contents):
        der = base64.b64decode(re.sub(rb"\s+", b"", match.group(1)), validate=True)
        fingerprints.add(hashlib.sha256(der).hexdigest())
    return fingerprints


def files_in(source: Path):
    if source.is_file():
        yield source
    elif source.is_dir():
        for directory, _, filenames in os.walk(source):
            for filename in filenames:
                yield Path(directory, filename)


def main() -> None:
    bundles = [path for path in BUNDLES if path.is_file()]
    if not bundles:
        raise SystemExit("no system CA bundle found")
    if not Path("/etc/ssl/certs").is_dir():
        raise SystemExit("system hashed certificate directory is missing")

    fingerprints: set[str] = set()
    scanned: set[Path] = set()
    for source in (*bundles, *SOURCES):
        for path in files_in(source):
            try:
                resolved = path.resolve(strict=True)
            except OSError:
                continue
            if resolved in scanned:
                continue
            scanned.add(resolved)
            fingerprints.update(certificate_fingerprints(resolved))

    if not fingerprints:
        raise SystemExit("no PEM certificates found in system trust sources")
    present = BLOCKED_ROOTS.keys() & fingerprints
    if present:
        raise SystemExit(
            "blocked eMudhra incident root(s) remain trusted: "
            + ", ".join(
                f"{BLOCKED_ROOTS[fingerprint]} ({fingerprint})"
                for fingerprint in sorted(present)
            )
        )


if __name__ == "__main__":
    main()
