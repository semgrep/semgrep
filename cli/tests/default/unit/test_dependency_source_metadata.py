"""Checks for authoritative scanner discovery metadata extraction."""
import unittest

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.dependency_source_metadata import collect_dependency_source_metadata

pytestmark = pytest.mark.quick


class TestDependencySourceMetadata(unittest.TestCase):
    def test_same_folder_manifest_sources_keep_distinct_paths(self) -> None:
        npm_source = out.DependencySource(
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.PackageJson()), out.Fpath("package.json")
                )
            )
        )
        python_source = out.DependencySource(
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.PyprojectToml()), out.Fpath("pyproject.toml")
                )
            )
        )
        npm_metadata = collect_dependency_source_metadata(npm_source)
        python_metadata = collect_dependency_source_metadata(python_source)
        self.assertEqual(npm_metadata.source_kind, "ManifestOnly")
        self.assertEqual(npm_metadata.manifest_paths, (out.Fpath("package.json"),))
        self.assertEqual(python_metadata.manifest_paths, (out.Fpath("pyproject.toml"),))
        self.assertNotEqual(npm_metadata, python_metadata)

    def test_manifest_lockfile_preserves_both_authoritative_paths(self) -> None:
        source = out.DependencySource(
            out.ManifestLockfile(
                (
                    out.Manifest(
                        out.ManifestKind(out.PackageJson()), out.Fpath("package.json")
                    ),
                    out.Lockfile(
                        out.LockfileKind(out.NpmPackageLockJson()),
                        out.Fpath("package-lock.json"),
                    ),
                )
            )
        )
        metadata = collect_dependency_source_metadata(source)
        self.assertEqual(metadata.source_kind, "ManifestLockfile")
        self.assertEqual(metadata.manifest_paths, (out.Fpath("package.json"),))
        self.assertEqual(metadata.lockfile_paths, (out.Fpath("package-lock.json"),))

    def test_lockfile_only_and_multi_sources_preserve_source_kind(self) -> None:
        lockfile_source = out.DependencySource(
            out.LockfileOnly(
                out.Lockfile(
                    out.LockfileKind(out.NpmPackageLockJson()),
                    out.Fpath("package-lock.json"),
                )
            )
        )
        metadata = collect_dependency_source_metadata(lockfile_source)
        self.assertEqual(metadata.source_kind, "LockfileOnly")
        self.assertEqual(metadata.manifest_paths, ())
        multi_metadata = collect_dependency_source_metadata(
            out.DependencySource(out.MultiLockfile([lockfile_source]))
        )
        self.assertEqual(multi_metadata.source_kind, "MultiLockfile")
        self.assertEqual(multi_metadata.lockfile_paths, metadata.lockfile_paths)

    def test_ephemeral_transport_does_not_replace_original_source_identity(
        self,
    ) -> None:
        original = out.DependencySource(
            out.ManifestOnly(
                out.Manifest(
                    out.ManifestKind(out.PackageJson()), out.Fpath("package.json")
                )
            )
        )
        wrapped = out.DependencySource(
            out.AuxillarySBOM(
                (
                    out.Sbom(
                        out.SbomKind(out.CycloneDXJson()),
                        True,
                        out.Fpath("/tmp/result.cdx.json"),
                    ),
                    original,
                )
            )
        )
        self.assertEqual(
            collect_dependency_source_metadata(wrapped),
            collect_dependency_source_metadata(original),
        )


if __name__ == "__main__":
    unittest.main()
