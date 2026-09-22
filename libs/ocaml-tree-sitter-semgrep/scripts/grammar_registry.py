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
"""Upstream grammar registry: pins, clone paths, and fetch."""
from __future__ import annotations

import argparse
import json
import subprocess
import sys
from functools import lru_cache
from pathlib import Path
from typing import Any
from typing import cast


# Dialect names from the old languages-* lists; not registry keys.
_ALIASES: dict[str, str] = {
    "soql": "sfapex",
    "sosl": "sfapex",
    "apex": "sfapex",
    "tsx": "typescript",
    "php-only": "php",
    "cfquery": "cfml",
    "cfscript": "cfml",
    "go-mod": "gomod",
}


class RegistryError(LookupError):
    """Invalid or missing registry entry."""


class UnknownGrammarError(RegistryError):
    """Unknown grammar or dialect name."""


def default_lang_dir() -> Path:
    # upstream-grammars.json lives under lang/; this module lives under scripts/.
    return Path(__file__).resolve().parent.parent / "lang"


def registry_path(lang_dir: Path | None = None) -> Path:
    return (lang_dir or default_lang_dir()) / "upstream-grammars.json"


def _read_registry(path: Path) -> dict[str, dict[str, Any]]:
    raw = json.loads(path.read_text())
    if not isinstance(raw, dict):
        raise RegistryError(f"{path}: registry root must be an object")
    return cast(dict[str, dict[str, Any]], raw)


def load_file(path: Path) -> dict[str, dict[str, Any]]:
    """Load and validate a registry file."""
    reg = _read_registry(path)
    validate(reg)
    return reg


@lru_cache(maxsize=1)
def _load_default() -> dict[str, dict[str, Any]]:
    return load_file(registry_path(default_lang_dir()))


def clear_load_cache() -> None:
    _load_default.cache_clear()


def load(lang_dir: Path | None = None) -> dict[str, dict[str, Any]]:
    if lang_dir is None:
        return _load_default()
    return load_file(registry_path(lang_dir))


def validate(reg: dict[str, dict[str, Any]]) -> None:
    dest_to_key: dict[str, str] = {}
    for key, entry in reg.items():
        for field in ("url", "commit", "tree_sitter", "regen"):
            if field not in entry:
                raise RegistryError(f"{key}: missing {field}")
        if not isinstance(entry["regen"], list):
            raise RegistryError(f"{key}: regen must be a list")
        for dep in entry.get("depends_on", []):
            if dep not in reg:
                raise RegistryError(f"{key}: unknown depends_on {dep}")
        for dest in entry["regen"]:
            if dest in dest_to_key:
                raise RegistryError(
                    f"duplicate regen dest {dest}: {dest_to_key[dest]} and {key}"
                )
            dest_to_key[dest] = key
    for key, entry in reg.items():
        for dep in entry.get("depends_on", []):
            _assert_no_cycle(reg, key, dep, {key})


def _assert_no_cycle(
    reg: dict[str, dict[str, Any]], start: str, node: str, seen: set[str]
) -> None:
    for dep in reg[node].get("depends_on", []):
        if dep == start:
            raise RegistryError(f"depends_on cycle involving {start}")
        if dep not in seen:
            _assert_no_cycle(reg, start, dep, seen | {dep})


def resolve_in_registry(name: str, registry: dict[str, dict[str, Any]]) -> str | None:
    """Resolve a key, destination, or alias in a loaded registry."""
    if name in registry:
        return name
    for key, entry in registry.items():
        if name in entry["regen"]:
            return key
    alias = _ALIASES.get(name)
    if alias is not None and alias in registry:
        return alias
    return None


def resolve(name: str, lang_dir: Path | None = None) -> str:
    key = resolve_in_registry(name, load(lang_dir))
    if key is not None:
        return key
    raise UnknownGrammarError(f"unknown grammar name: {name}")


def dests(key: str, lang_dir: Path | None = None) -> list[str]:
    return list(load(lang_dir)[key]["regen"])


def clone_name(key: str, lang_dir: Path | None = None) -> str:
    return cast(str, load(lang_dir)[key].get("clone", key))


def wrapper_dir(key: str, lang_dir: Path | None = None) -> str:
    return f"semgrep-{key}"


def grammar_test_targets(lang_dir: Path | None = None) -> list[str]:
    """Return registry keys accepted by test-lang.

    Every semgrep-* wrapper must have a matching registry key; an orphaned
    wrapper means a forgotten registry entry.
    """
    lang_dir = lang_dir or default_lang_dir()
    src = lang_dir / "semgrep-grammars" / "src"
    wrappers = {
        p.name.removeprefix("semgrep-") for p in src.glob("semgrep-*") if p.is_dir()
    }
    registry = load(lang_dir)
    orphaned = sorted(wrappers - registry.keys())
    if orphaned:
        raise RegistryError(
            f"grammar wrappers with no registry entry: {', '.join(orphaned)}"
        )
    return sorted(registry.keys() & wrappers)


def entry_for(name: str, lang_dir: Path | None = None) -> tuple[str, dict[str, Any]]:
    key = resolve(name, lang_dir)
    return key, load(lang_dir)[key]


def lang_dir_for_dest(ots_root: Path, dest: str) -> Path:
    key = resolve(dest)
    candidate = ots_root / "lang" / dest
    if candidate.is_dir():
        return candidate
    return ots_root / "lang" / key


def clone_for_wrapper_name(wrapper_name: str, lang_dir: Path | None = None) -> str:
    """Map semgrep-* dirname (e.g. gomod) to tree-sitter clone dir name."""
    key = resolve(wrapper_name, lang_dir)
    return clone_name(key, lang_dir)


def _https_url(url: str) -> str:
    return url.replace("git@github.com:", "https://github.com/")


def _is_empty_dir(path: Path) -> bool:
    return not path.is_dir() or not any(path.iterdir())


def _git_run(cwd: Path, *args: str) -> None:
    r = subprocess.run(
        ["git", "-C", str(cwd), *args],
        capture_output=True,
        text=True,
    )
    if r.returncode != 0:
        sys.stderr.write(r.stderr)
        raise subprocess.CalledProcessError(r.returncode, r.args, r.stdout, r.stderr)


def fetch(ots_root: Path, key: str, lang_dir: Path | None = None) -> Path:
    entry = load(lang_dir)[key]
    clone = clone_name(key, lang_dir)
    dest = ots_root / "lang" / "semgrep-grammars" / "src" / f"tree-sitter-{clone}"
    url = _https_url(entry["url"])
    pin = entry["commit"]
    if _is_empty_dir(dest):
        dest.parent.mkdir(parents=True, exist_ok=True)
        if dest.exists():
            dest.rmdir()
        subprocess.run(
            ["git", "clone", "--quiet", url, str(dest)],
            check=True,
        )
    # Reuse cached immutable commits; refresh symbolic refs.
    is_commit = len(pin) == 40 and all(c in "0123456789abcdefABCDEF" for c in pin)
    cached = (
        is_commit
        and subprocess.run(
            ["git", "-C", str(dest), "cat-file", "-e", f"{pin}^{{commit}}"],
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL,
        ).returncode
        == 0
    )
    if not cached:
        _git_run(dest, "fetch", "--quiet", "origin", pin)
    _git_run(dest, "checkout", "--quiet", pin)
    return dest


def fetch_with_deps(ots_root: Path, name: str, lang_dir: Path | None = None) -> str:
    key = resolve(name, lang_dir)
    reg = load(lang_dir)
    for dep in reg[key].get("depends_on", []):
        fetch_with_deps(ots_root, dep, lang_dir)
    fetch(ots_root, key, lang_dir)
    return key


def _main() -> int:
    parser = argparse.ArgumentParser(description="Grammar registry utilities")
    sub = parser.add_subparsers(dest="cmd", required=True)
    sub.add_parser("validate")
    fetch_p = sub.add_parser("fetch")
    fetch_p.add_argument("name")
    clone_p = sub.add_parser("clone-for-wrapper")
    clone_p.add_argument("wrapper_name", help="semgrep-* basename without prefix")
    wrapper_p = sub.add_parser("wrapper-for-lang")
    wrapper_p.add_argument("name")
    args = parser.parse_args()
    lang_dir = default_lang_dir()
    ots_root = lang_dir.parent
    if args.cmd == "validate":
        load(lang_dir)
        return 0
    if args.cmd == "fetch":
        fetch_with_deps(ots_root, args.name, lang_dir)
        return 0
    if args.cmd == "wrapper-for-lang":
        print(wrapper_dir(resolve(args.name, lang_dir), lang_dir))
        return 0
    if args.cmd == "clone-for-wrapper":
        print(clone_for_wrapper_name(args.wrapper_name, lang_dir))
        return 0
    return 2


if __name__ == "__main__":
    raise SystemExit(_main())
