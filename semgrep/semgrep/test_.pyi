from typing import Any, Dict, List, Sequence, Set, Tuple

from pathlib import Path

"""
For each directory containing YAML rules, run those rules on the file in the same directory with the same name but different extension.
E.g. eqeq.yaml runs on eqeq.py.
Validate that the output is annotated in the source file with by looking for a comment like:

 ```
 # ruleid:eqeq-is-bad
 ```
 On the preceeding line.

 """

# TODO: test_main() ??

def get_config_filenames(original_config: Path) -> List[Path]: ...
def get_config_test_filenames(
    original_config: Path, configs: List[Path], original_target: Path
) -> Dict[Path, List[Path]]: ...
