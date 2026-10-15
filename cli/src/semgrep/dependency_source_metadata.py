"""Describe discovered dependency sources without inferring identity from folders."""
from dataclasses import dataclass

import semgrep.semgrep_interfaces.semgrep_output_v1 as out


@dataclass(frozen=True)
class DependencySourceMetadata:
    source_kind: str
    manifest_paths: tuple[out.Fpath, ...]
    lockfile_paths: tuple[out.Fpath, ...]


def collect_dependency_source_metadata(
    dependency_source: out.DependencySource,
) -> DependencySourceMetadata:
    """Preserve the scanner's source classification and exact source paths."""
    source = dependency_source.value
    if isinstance(source, out.ManifestOnly):
        return DependencySourceMetadata(source.kind, (source.value.path,), ())
    if isinstance(source, out.LockfileOnly):
        return DependencySourceMetadata(source.kind, (), (source.value.path,))
    if isinstance(source, out.ManifestLockfile):
        return DependencySourceMetadata(
            source.kind, (source.value[0].path,), (source.value[1].path,)
        )
    if isinstance(source, out.AuxillarySBOM):
        return collect_dependency_source_metadata(source.value[1])
    if isinstance(source, out.MultiLockfile):
        descriptions = [
            collect_dependency_source_metadata(child) for child in source.value
        ]
        return DependencySourceMetadata(
            source.kind,
            tuple(path for item in descriptions for path in item.manifest_paths),
            tuple(path for item in descriptions for path in item.lockfile_paths),
        )
    raise ValueError(f"Unsupported dependency source kind: {source.kind}")
