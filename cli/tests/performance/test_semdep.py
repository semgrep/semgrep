from pathlib import Path
from time import time

import pytest

from semdep.parsers.gem import parse_gemfile
from semdep.parsers.go_mod import parse_go_mod
from semdep.parsers.gradle import parse_gradle
from semdep.parsers.package_lock import parse_package_lock
from semdep.parsers.pipfile import parse_pipfile
from semdep.parsers.poetry import parse_poetry
from semdep.parsers.pom_tree import parse_pom_tree
from semdep.parsers.requirements import parse_requirements
from semdep.parsers.util import SemgrepParser
from semdep.parsers.yarn import parse_yarn


@pytest.mark.kinda_slow
@pytest.mark.parametrize(
    "file_size,target,max_time,parser",
    [
        (file_size, target, max_time, parser)
        # These times are set relative to Github Actions, they should be lower when
        # running locally.
        # Local time expectation is more like 1, 5, 10
        # update: 10k is actually taking more time locally, 3.5 on pad's Linux machine
        for file_size, max_time in [("10k", 4), ("50k", 15), ("100k", 30)]
        for target, parser in [
            ("Gemfile.lock", parse_gemfile),
            ("go.mod", parse_go_mod),
            ("gradle.lockfile", parse_gradle),
            ("maven_dep_tree.txt", parse_pom_tree),
            ("package-lock.json", parse_package_lock),
            ("poetry.lock", parse_poetry),
            ("requirements.txt", parse_requirements),
            ("yarn.lock", parse_yarn),
            ("Pipfile.lock", parse_pipfile),
        ]
    ],
)
def test_dependency_aware_timing(
    lockfile_path_in_tmp_for_perf,
    file_size: str,
    target: str,
    max_time: int,
    parser: SemgrepParser,
):
    start = time()

    lockfile_path = Path(f"targets_perf_sca/{file_size}/{target}")
    _, error = parser(lockfile_path, None)

    end = time()

    assert len(error) == 0
    exec_time = end - start
    assert exec_time < max_time
