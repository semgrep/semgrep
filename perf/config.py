from typing import List
from pathlib import Path
import attr
from ruamel.yaml import YAML
import time

import logging
import sys

logger = logging.getLogger(__file__)
logger.setLevel(logging.DEBUG)
handler = logging.StreamHandler(stream=sys.stderr)
handler.setFormatter(logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s'))
logger.addHandler(handler)

yaml = YAML(typ='rt')

@attr.s
class Repository(object):
    url: str = attr.ib(default="")
    commit_hash: str = attr.ib(default="HEAD")

@attr.s
class BenchmarkRunSetupData(object):
    """
    Stores data about an individual benchmark run
    """

    run_name: str = attr.ib(default='benchmark_run')
    rule_configs: List[str] = attr.ib(default=["p/all"])
    repositories: List[Repository] = attr.ib(factory=list)
    semgrep_options: List[str] = attr.ib(factory=list)


@attr.s
class SemgrepBenchmarkConfig(object):
    """
    Stores data needed to start a benchmarking run.

    YAML structure:
    '''
    runs:
    - run_name1:
        repos:
        - url: <url>
          commit_hash: <commit_hash>
        - url: <url>
          commit_hash: <commit_hash>
        rule_configs:
        - <string used with 'semgrep -f', e.g., p/xss>
        opts: <semgrep CLI flags>
    - run_name2:
        ...
    '''
    """

    benchmark_setup_data: List[BenchmarkRunSetupData] = attr.ib(factory=list)

    @classmethod
    def parse_config(cls, config_file: Path):
        logger.debug(f"Using config at {config_file.absolute()}")
        with open(config_file, 'r') as fin:
            config = yaml.load(fin)

        return SemgrepBenchmarkConfig([
            BenchmarkRunSetupData(
                run_name=config_data.get('name', f"run-{int(time.time())}"),
                rule_configs = config_data.get('rule_configs', []),
                repositories = [
                    Repository(repo.get('url'), repo.get('commit_hash'))
                    for repo in config_data.get('repos', [])
                ],
                semgrep_options = config_data.get('opts', [])
            ) for config_data in config.get('runs', [])
        ])