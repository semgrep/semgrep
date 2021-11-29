#!/usr/bin/env python3
import json
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from typing import Collection
from typing import Mapping

from jinja2 import Environment
from jinja2 import FileSystemLoader
from jinja2 import select_autoescape


# Generates Lang.ml from Lang.ml.j2 and the shared lang.json data structure.
# See github.com/returntocorp/semgrep-langs for more information


# These languages are not covered by Lang.ml
EXTERNAL_LANGS = ["generic", "regex"]


@dataclass(frozen=True)
class LanguageDefinition:
    """
    Mirrors schema of lang.json (see lang/README.md) for each language
    """

    id: str
    name: str
    keys: Collection[str]
    exts: Collection[str]
    excluded_exts: Collection[str]
    reverse_exts: Collection[str]
    shebangs: Collection[str]
    tags: Collection[str]

    @classmethod
    def from_dict(cls, data: Mapping[str, Any]) -> "LanguageDefinition":
        return cls(
            # "id" is mapped to OCaml types, which must begin with a capital letter
            id=data["id"].capitalize(),
            name=data["name"],
            keys=data["keys"],
            exts=data["exts"],
            excluded_exts=data.get("excluded_exts", []),
            reverse_exts=data.get("reverse_exts", data["exts"]),
            shebangs=data.get("shebangs", []),
            tags=data.get("tags", []),
        )


with (Path(__file__).parent / "lang" / "lang.json").open() as fd:
    data = json.load(fd)


langs = {
    d["id"]: LanguageDefinition.from_dict(d)
    for d in data
    if d["id"] not in EXTERNAL_LANGS
}

env = Environment(
    loader=FileSystemLoader(str(Path(__file__).parent)), autoescape=select_autoescape()
)

for f in ["Lang.ml", "Lang.mli"]:
    print(f"Generating {f}...")
    data = env.get_template(f"{f}.j2").render(langs=langs.values())
    with (Path(__file__).parent / f).open("w") as fd:
        fd.write(data)
    print("...done.")
