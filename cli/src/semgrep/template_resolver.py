from contextlib import contextmanager
from typing import List
from typing import Dict
from urllib import request
import os
import shutil

from semgrep.util import is_url
from semgrep.rule import Rule
from pathlib import Path

from tempfile import TemporaryDirectory
from semgrep.core_runner import get_state

template_file_id = 0

def uniq_id() -> int:
    global template_file_id
    template_file_id += 1
    return template_file_id

def resolve_template_urls(rules : List[Rule], template_dir : Path) -> None:
    """
     TODO: local files can download templates. This should mutate metrics
           state
     TODO: templates break the invariant the registry rules will not cause
           latest semgrep to error because they could error after being
           compiled. In particular, even if the rule initially is valid, it
           could become broken after a template is changed. This error will
           be extremely hard to parse
    """
    # Retrieve the paths to any templates used by the rules

    template_defs = [ rule._raw["uses"] if "uses" in rule._raw else [] for rule in rules ]

    template_paths = { path for uses in template_defs for use_def in uses for path in use_def.values() }

    # Download the templates as needed; write back the name of the file
    # so that the rule sent to semgrep-core will reference the file

    """
     TODO: pulling templates is going to be slow. Instead of 
     delivering them through the registry, perhaps we should
     deliver them as a package that people can install. This
     also improves the versioning problem, though it unfortunately
     puts the burden of installing on users
    """

    template_mappings = {}
    for path in template_paths:
        if is_url(path):
            filepath = str(template_dir / ("template_" + str(uniq_id()) + "_" + os.path.basename(path)))
            request.urlretrieve(path, filepath)
            template_mappings[path] = filepath
        else:
            # Copy local files so that later semgrep-core knows which folder
            # to put the equivalent libsonnet file in. A bit wasteful, but
            # this will also be nice for debugging
            _dirname, basename = os.path.split(path)
            filepath = str(template_dir / ("template_" + str(uniq_id()) + "_" + basename))
            shutil.copyfile(path, filepath, follow_symlinks=True)
            template_mappings[path] = filepath

    def replace_urls_in_place(uses : List[Dict[str, str]]) -> int:
        for use in uses:
            for (template_id, template_path) in use.items():
                use[template_id] = template_mappings[template_path]
        return 0 # for mypy, since this intentionally uses mutation

    [ replace_urls_in_place(rule._raw["uses"]) if "uses" in rule._raw else () for rule in rules ]

class TemplateDirectory(TemporaryDirectory):
    temporary = True
    name = ""

    def __init__(self, command_for_core : bool):
        self.temporary = not command_for_core
        if self.temporary:
            super().__init__()

    def __enter__(self):
        if self.temporary:
            name = super().__enter__()
            self.name = name
            return self
        else:
            # Create a directory
            state = get_state()
            self.name = state.env.user_data_folder / "semgrep_templates"
            if not os.path.isdir(self.name):
                os.mkdir(self.name)
            return self

    def __exit__(self, exc, value, tb):
        if self.temporary:
            return super().__exit__(exc, value, tb)
        else:
            return