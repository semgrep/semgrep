#
# Copyright (c) 2025 Semgrep Inc.
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
from pathlib import Path
from unittest.mock import patch

import pytest

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.semgrep_interfaces.semgrep_output_v1 import Ecosystem
from semgrep.semgrep_interfaces.semgrep_output_v1 import Gomod
from semgrep.semgrep_interfaces.semgrep_output_v1 import Maven
from semgrep.semgrep_interfaces.semgrep_output_v1 import Npm
from semgrep.semgrep_interfaces.semgrep_output_v1 import Pypi
from semgrep.subproject import from_resolved_dependencies
from semgrep.symbol_analysis import _ecosystem_to_language
from semgrep.symbol_analysis import build_subproject_file_mapping
from semgrep.symbol_analysis import run_subproject_symbol_analysis
from semgrep.symbol_analysis import run_symbol_analysis_for_files
from semgrep.target_manager import TargetManager


def make_symbol_usage(fqn: list[str]) -> out.SymbolUsage:
    """Helper to create a SymbolUsage for testing."""
    return out.SymbolUsage(
        symbol=out.Symbol(fqn=fqn),
        locs=[
            out.Location(
                path=out.Fpath("test.py"),
                start=out.Position(line=1, col=1, offset=0),
                end=out.Position(line=1, col=10, offset=9),
            )
        ],
    )


def make_pypi_subproject(root_dir: str, lockfile_path: str) -> out.ResolvedSubproject:
    """Helper to create a Pypi ResolvedSubproject for testing."""
    return out.ResolvedSubproject(
        info=out.Subproject(
            root_dir=out.Fpath(root_dir),
            dependency_source=out.DependencySource(
                out.LockfileOnly(
                    out.Lockfile(
                        out.LockfileKind(out.PipRequirementsTxt()),
                        out.Fpath(lockfile_path),
                    )
                )
            ),
            ecosystem=Ecosystem(Pypi()),
        ),
        resolution_method=out.ResolutionMethod(out.LockfileParsing()),
        ecosystem=Ecosystem(Pypi()),
        resolved_dependencies=from_resolved_dependencies([]),
        errors=[],
    )


def make_npm_subproject(
    root_dir: str, lockfile_path: str, manifest_path: str
) -> out.ResolvedSubproject:
    """Helper to create an Npm ResolvedSubproject for testing."""
    return out.ResolvedSubproject(
        info=out.Subproject(
            root_dir=out.Fpath(root_dir),
            dependency_source=out.DependencySource(
                out.ManifestLockfile(
                    (
                        out.Manifest(
                            out.ManifestKind(out.PackageJson()),
                            out.Fpath(manifest_path),
                        ),
                        out.Lockfile(
                            out.LockfileKind(out.NpmPackageLockJson()),
                            out.Fpath(lockfile_path),
                        ),
                    )
                )
            ),
            ecosystem=Ecosystem(Npm()),
        ),
        resolution_method=out.ResolutionMethod(out.LockfileParsing()),
        ecosystem=Ecosystem(Npm()),
        resolved_dependencies=from_resolved_dependencies([]),
        errors=[],
    )


def make_maven_subproject(root_dir: str, lockfile_path: str) -> out.ResolvedSubproject:
    """Helper to create a Maven ResolvedSubproject for testing."""
    return out.ResolvedSubproject(
        info=out.Subproject(
            root_dir=out.Fpath(root_dir),
            dependency_source=out.DependencySource(
                out.LockfileOnly(
                    out.Lockfile(
                        out.LockfileKind(out.GradleLockfile()),
                        out.Fpath(lockfile_path),
                    )
                )
            ),
            ecosystem=Ecosystem(Maven()),
        ),
        resolution_method=out.ResolutionMethod(out.LockfileParsing()),
        ecosystem=Ecosystem(Maven()),
        resolved_dependencies=from_resolved_dependencies([]),
        errors=[],
    )


class TestEcosystemToLanguage:
    @pytest.mark.quick
    def test_pypi_returns_python(self):
        ecosystem = Ecosystem(Pypi())
        assert _ecosystem_to_language(ecosystem) == "python"

    @pytest.mark.quick
    def test_npm_returns_js(self):
        ecosystem = Ecosystem(Npm())
        assert _ecosystem_to_language(ecosystem) == "js"

    @pytest.mark.quick
    def test_maven_returns_none(self):
        # Maven can be java, scala, kotlin - so returns None
        ecosystem = Ecosystem(Maven())
        assert _ecosystem_to_language(ecosystem) is None

    @pytest.mark.quick
    def test_gomod_returns_none(self):
        ecosystem = Ecosystem(Gomod())
        assert _ecosystem_to_language(ecosystem) is None


class TestBuildSubprojectFileMapping:
    @pytest.mark.quick
    def test_empty_subprojects_returns_empty_mapping(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Empty subprojects dict returns empty mapping."""
        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )
        subprojects_by_ecosystem: dict[Ecosystem, list[out.ResolvedSubproject]] = {}

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        assert result == {}

    @pytest.mark.quick
    def test_unsupported_ecosystem_skipped(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Ecosystems without language mappings (e.g., Maven) should be skipped."""
        # Create a Java file and gradle lockfile
        (tmp_path / "project").mkdir()
        (tmp_path / "project" / "App.java").touch()
        (tmp_path / "project" / "gradle.lockfile").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject = make_maven_subproject("project", "project/gradle.lockfile")
        subprojects_by_ecosystem = {Ecosystem(Maven()): [subproject]}

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        # Maven returns None for language, so should return empty mapping
        assert result == {}

    @pytest.mark.quick
    def test_pypi_subproject_maps_files_to_subproject(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Files should be mapped to their Pypi subproject."""
        # Create test files
        (tmp_path / "src").mkdir()
        (tmp_path / "src" / "main.py").touch()
        (tmp_path / "src" / "utils.py").touch()
        (tmp_path / "requirements.txt").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject = make_pypi_subproject(".", "requirements.txt")
        subprojects_by_ecosystem = {Ecosystem(Pypi()): [subproject]}

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        # Check that files are mapped to the subproject
        key = (Ecosystem(Pypi()), Path("."))
        assert key in result
        # Verify the mapping contains the Python files we care about
        result_paths = {p.resolve() for p in result[key]}
        assert (tmp_path / "src" / "main.py").resolve() in result_paths
        assert (tmp_path / "src" / "utils.py").resolve() in result_paths

    @pytest.mark.quick
    def test_multiple_subprojects_files_mapped_to_closest(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Files should be mapped to their closest (deepest) subproject."""
        # Create directory structure with nested subprojects
        (tmp_path / "root").mkdir()
        (tmp_path / "root" / "requirements.txt").touch()
        (tmp_path / "root" / "app.py").touch()
        (tmp_path / "root" / "nested").mkdir()
        (tmp_path / "root" / "nested" / "requirements.txt").touch()
        (tmp_path / "root" / "nested" / "nested_app.py").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        root_subproject = make_pypi_subproject("root", "root/requirements.txt")
        nested_subproject = make_pypi_subproject(
            "root/nested", "root/nested/requirements.txt"
        )

        subprojects_by_ecosystem = {
            Ecosystem(Pypi()): [root_subproject, nested_subproject]
        }

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        # root/app.py should be in root subproject
        root_key = (Ecosystem(Pypi()), Path("root"))
        assert root_key in result
        root_paths = {p.resolve() for p in result[root_key]}
        assert (tmp_path / "root" / "app.py").resolve() in root_paths

        # root/nested/nested_app.py should be in nested subproject
        nested_key = (Ecosystem(Pypi()), Path("root/nested"))
        assert nested_key in result
        nested_paths = {p.resolve() for p in result[nested_key]}
        assert (
            tmp_path / "root" / "nested" / "nested_app.py"
        ).resolve() in nested_paths

    @pytest.mark.quick
    def test_npm_subproject_maps_js_files(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """JavaScript files should be mapped to their NPM subproject."""
        (tmp_path / "app.js").touch()
        (tmp_path / "utils.ts").touch()
        (tmp_path / "package-lock.json").touch()
        (tmp_path / "package.json").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject = make_npm_subproject(".", "package-lock.json", "package.json")
        subprojects_by_ecosystem = {Ecosystem(Npm()): [subproject]}

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        key = (Ecosystem(Npm()), Path("."))
        assert key in result
        # JS language should pick up both .js and .ts files
        result_paths = {p.resolve() for p in result[key]}
        assert (tmp_path / "app.js").resolve() in result_paths

    @pytest.mark.quick
    def test_files_outside_subproject_not_mapped(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Files outside of a subproject's root should not be mapped to it."""
        # Create files in different locations
        (tmp_path / "outside.py").touch()
        (tmp_path / "project").mkdir()
        (tmp_path / "project" / "inside.py").touch()
        (tmp_path / "project" / "requirements.txt").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        # Create subproject rooted at "project/"
        subproject = make_pypi_subproject("project", "project/requirements.txt")
        subprojects_by_ecosystem = {Ecosystem(Pypi()): [subproject]}

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        # Only files under "project/" should be mapped
        key = (Ecosystem(Pypi()), Path("project"))
        assert key in result
        result_paths = {p.resolve() for p in result[key]}
        assert (tmp_path / "project" / "inside.py").resolve() in result_paths
        # outside.py should NOT be mapped to the project subproject
        assert (tmp_path / "outside.py").resolve() not in result_paths

    @pytest.mark.quick
    def test_pypi_only_maps_python_files(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Pypi subprojects should only map Python files, not JS or other language files."""
        (tmp_path / "app.py").touch()
        (tmp_path / "main.js").touch()
        (tmp_path / "Main.java").touch()
        (tmp_path / "lib.go").touch()
        (tmp_path / "requirements.txt").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject = make_pypi_subproject(".", "requirements.txt")
        subprojects_by_ecosystem = {Ecosystem(Pypi()): [subproject]}

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        key = (Ecosystem(Pypi()), Path("."))
        assert key in result
        result_paths = {p.resolve() for p in result[key]}

        # Should contain Python files only
        assert (tmp_path / "app.py").resolve() in result_paths

        # Should NOT contain non-Python files
        assert (tmp_path / "main.js").resolve() not in result_paths
        assert (tmp_path / "Main.java").resolve() not in result_paths
        assert (tmp_path / "lib.go").resolve() not in result_paths

    @pytest.mark.quick
    def test_npm_only_maps_js_files(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """NPM subprojects should only map JS/TS files, not Python or other language files."""
        (tmp_path / "app.js").touch()
        (tmp_path / "utils.ts").touch()
        (tmp_path / "main.py").touch()
        (tmp_path / "Main.java").touch()
        (tmp_path / "package-lock.json").touch()
        (tmp_path / "package.json").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject = make_npm_subproject(".", "package-lock.json", "package.json")
        subprojects_by_ecosystem = {Ecosystem(Npm()): [subproject]}

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        key = (Ecosystem(Npm()), Path("."))
        assert key in result
        result_paths = {p.resolve() for p in result[key]}

        # Should contain JS/TS files only
        assert (tmp_path / "app.js").resolve() in result_paths
        # Note: whether .ts is included depends on the JS language definition

        # Should NOT contain non-JS files
        assert (tmp_path / "main.py").resolve() not in result_paths
        assert (tmp_path / "Main.java").resolve() not in result_paths

    @pytest.mark.quick
    def test_mixed_ecosystems_map_correct_files(
        self, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """When multiple ecosystems exist, each should only map files for their language."""
        (tmp_path / "backend").mkdir()
        (tmp_path / "backend" / "server.py").touch()
        (tmp_path / "backend" / "requirements.txt").touch()
        (tmp_path / "frontend").mkdir()
        (tmp_path / "frontend" / "app.js").touch()
        (tmp_path / "frontend" / "package-lock.json").touch()
        (tmp_path / "frontend" / "package.json").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        pypi_subproject = make_pypi_subproject("backend", "backend/requirements.txt")
        npm_subproject = make_npm_subproject(
            "frontend", "frontend/package-lock.json", "frontend/package.json"
        )

        subprojects_by_ecosystem = {
            Ecosystem(Pypi()): [pypi_subproject],
            Ecosystem(Npm()): [npm_subproject],
        }

        result = build_subproject_file_mapping(subprojects_by_ecosystem, target_manager)

        # Pypi subproject should only have Python files
        pypi_key = (Ecosystem(Pypi()), Path("backend"))
        assert pypi_key in result
        pypi_paths = {p.resolve() for p in result[pypi_key]}
        assert (tmp_path / "backend" / "server.py").resolve() in pypi_paths
        assert (tmp_path / "frontend" / "app.js").resolve() not in pypi_paths

        # NPM subproject should only have JS files
        npm_key = (Ecosystem(Npm()), Path("frontend"))
        assert npm_key in result
        npm_paths = {p.resolve() for p in result[npm_key]}
        assert (tmp_path / "frontend" / "app.js").resolve() in npm_paths
        assert (tmp_path / "backend" / "server.py").resolve() not in npm_paths


class TestRunSymbolAnalysisForFiles:
    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_calls_rpc_with_correct_params(self, mock_rpc):
        """Should call RPC with proper SymbolAnalysisParams."""
        expected_result = out.SymbolAnalysis(value=[])
        mock_rpc.return_value = expected_result

        root_path = Path("/project")
        files = [Path("/project/main.py"), Path("/project/utils.py")]

        result = run_symbol_analysis_for_files(
            root_path=root_path,
            lang="python",
            files=files,
        )

        # Verify RPC was called
        mock_rpc.assert_called_once()
        call_args = mock_rpc.call_args
        params = call_args.kwargs["params"]

        assert params.root_path.value == "/project"
        assert params.lang == "python"
        assert len(params.files) == 2
        assert params.files[0].value == "/project/main.py"
        assert params.files[1].value == "/project/utils.py"

        assert result == expected_result

    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_returns_none_when_rpc_fails(self, mock_rpc):
        """Should return None when RPC call fails."""
        mock_rpc.return_value = None

        result = run_symbol_analysis_for_files(
            root_path=Path("/project"),
            lang="python",
            files=[Path("/project/main.py")],
        )

        assert result is None

    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_returns_symbol_analysis_with_usages(self, mock_rpc):
        """Should return SymbolAnalysis with symbol usages."""
        symbol_usage = make_symbol_usage(["requests", "get"])
        expected_result = out.SymbolAnalysis(value=[symbol_usage])
        mock_rpc.return_value = expected_result

        result = run_symbol_analysis_for_files(
            root_path=Path("/project"),
            lang="python",
            files=[Path("/project/main.py")],
        )

        assert result is not None
        assert len(result.value) == 1
        assert result.value[0].symbol.fqn == ["requests", "get"]


class TestRunScaSymbolAnalysis:
    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_returns_empty_analysis_when_no_subprojects(
        self, mock_rpc, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Should return empty SymbolAnalysis when no subprojects provided."""
        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        result = run_subproject_symbol_analysis({}, target_manager)

        assert result.value == []
        mock_rpc.assert_not_called()

    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_skips_unsupported_ecosystems(
        self, mock_rpc, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Ecosystems without language support should be skipped."""
        (tmp_path / "project").mkdir()
        (tmp_path / "project" / "App.java").touch()
        (tmp_path / "project" / "gradle.lockfile").touch()

        monkeypatch.chdir(tmp_path)

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject = make_maven_subproject("project", "project/gradle.lockfile")
        subprojects_by_ecosystem = {Ecosystem(Maven()): [subproject]}

        result = run_subproject_symbol_analysis(
            subprojects_by_ecosystem, target_manager
        )

        assert result.value == []
        mock_rpc.assert_not_called()

    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_skips_subprojects_outside_scanning_roots(
        self, mock_rpc, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Subprojects outside the scanning roots should be skipped (no files found)."""
        # Create a subproject directory but scan a different directory
        (tmp_path / "scanned").mkdir()
        (tmp_path / "scanned" / "app.py").touch()
        (tmp_path / "not_scanned").mkdir()
        (tmp_path / "not_scanned" / "other.py").touch()
        (tmp_path / "not_scanned" / "requirements.txt").touch()

        monkeypatch.chdir(tmp_path)

        # Scan only "scanned/" directory
        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path("scanned")]),
        )

        # But create a subproject for "not_scanned/"
        subproject = make_pypi_subproject("not_scanned", "not_scanned/requirements.txt")
        subprojects_by_ecosystem = {Ecosystem(Pypi()): [subproject]}

        result = run_subproject_symbol_analysis(
            subprojects_by_ecosystem, target_manager
        )

        # No files should be found for the subproject since it's outside scanning roots
        assert result.value == []
        mock_rpc.assert_not_called()

    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_calls_rpc_for_subproject_with_files(
        self, mock_rpc, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Should call RPC for subprojects that have matching files."""
        (tmp_path / "main.py").touch()
        (tmp_path / "requirements.txt").touch()

        monkeypatch.chdir(tmp_path)

        usage = make_symbol_usage(["requests", "get"])
        mock_rpc.return_value = out.SymbolAnalysis(value=[usage])

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject = make_pypi_subproject(".", "requirements.txt")
        subprojects_by_ecosystem = {Ecosystem(Pypi()): [subproject]}

        result = run_subproject_symbol_analysis(
            subprojects_by_ecosystem, target_manager
        )

        mock_rpc.assert_called_once()
        params = mock_rpc.call_args.kwargs["params"]
        assert params.lang == "python"
        assert len(result.value) == 1
        assert result.value[0].symbol.fqn == ["requests", "get"]

    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_combines_results_from_multiple_subprojects(
        self, mock_rpc, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Results from multiple subprojects should be combined."""
        (tmp_path / "app1").mkdir()
        (tmp_path / "app1" / "main.py").touch()
        (tmp_path / "app1" / "requirements.txt").touch()
        (tmp_path / "app2").mkdir()
        (tmp_path / "app2" / "main.py").touch()
        (tmp_path / "app2" / "requirements.txt").touch()

        monkeypatch.chdir(tmp_path)

        # Create two symbol usages
        usage1 = make_symbol_usage(["requests", "get"])
        usage2 = make_symbol_usage(["flask", "Flask"])

        # Mock RPC to return different results for each call
        mock_rpc.side_effect = [
            out.SymbolAnalysis(value=[usage1]),
            out.SymbolAnalysis(value=[usage2]),
        ]

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject1 = make_pypi_subproject("app1", "app1/requirements.txt")
        subproject2 = make_pypi_subproject("app2", "app2/requirements.txt")

        subprojects_by_ecosystem = {Ecosystem(Pypi()): [subproject1, subproject2]}

        result = run_subproject_symbol_analysis(
            subprojects_by_ecosystem, target_manager
        )

        # Should have combined both usages
        assert len(result.value) == 2
        fqns = [u.symbol.fqn for u in result.value]
        assert ["requests", "get"] in fqns
        assert ["flask", "Flask"] in fqns

    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_handles_rpc_failure_gracefully(
        self, mock_rpc, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """Should handle RPC failures gracefully and continue with other subprojects."""
        (tmp_path / "app1").mkdir()
        (tmp_path / "app1" / "main.py").touch()
        (tmp_path / "app1" / "requirements.txt").touch()
        (tmp_path / "app2").mkdir()
        (tmp_path / "app2" / "main.py").touch()
        (tmp_path / "app2" / "requirements.txt").touch()

        monkeypatch.chdir(tmp_path)

        usage = make_symbol_usage(["requests", "get"])

        # First call fails, second succeeds
        mock_rpc.side_effect = [None, out.SymbolAnalysis(value=[usage])]

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject1 = make_pypi_subproject("app1", "app1/requirements.txt")
        subproject2 = make_pypi_subproject("app2", "app2/requirements.txt")

        subprojects_by_ecosystem = {Ecosystem(Pypi()): [subproject1, subproject2]}

        result = run_subproject_symbol_analysis(
            subprojects_by_ecosystem, target_manager
        )

        # Should still have the result from the second subproject
        assert len(result.value) == 1
        assert result.value[0].symbol.fqn == ["requests", "get"]

    @pytest.mark.quick
    @patch("semgrep.symbol_analysis.run_symbol_analysis_rpc")
    def test_npm_subproject_uses_js_language(
        self, mock_rpc, tmp_path: Path, monkeypatch: pytest.MonkeyPatch
    ):
        """NPM subprojects should use 'js' as the language."""
        (tmp_path / "app.js").touch()
        (tmp_path / "package-lock.json").touch()
        (tmp_path / "package.json").touch()

        monkeypatch.chdir(tmp_path)

        mock_rpc.return_value = out.SymbolAnalysis(value=[])

        target_manager = TargetManager(
            scanning_root_strings=frozenset([Path(".")]),
        )

        subproject = make_npm_subproject(".", "package-lock.json", "package.json")
        subprojects_by_ecosystem = {Ecosystem(Npm()): [subproject]}

        run_subproject_symbol_analysis(subprojects_by_ecosystem, target_manager)

        # Verify language passed to RPC is 'js'
        mock_rpc.assert_called_once()
        params = mock_rpc.call_args.kwargs["params"]
        assert params.lang == "js"
