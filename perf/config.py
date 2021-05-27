import logging
import sys
import time
from pathlib import Path
from typing import List
from typing import Optional
from urllib.parse import urlparse

import attr
import requests
from constants import SEMGREP_URL
from constants import SEMGREP_USER_AGENT
from ruamel.yaml import YAML


logger = logging.getLogger(__file__)
logger.setLevel(logging.DEBUG)
handler = logging.StreamHandler(stream=sys.stderr)
handler.setFormatter(
    logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
)
logger.addHandler(handler)

yaml = YAML(typ="rt")


@attr.s
class Repository(object):
    url: str = attr.ib(default="")
    commit_hash: str = attr.ib(default="HEAD")


@attr.s
class RuleConfig(object):
    config_str: str = attr.ib(default="")

    def _fetch_rule_config_from_url(self, rule_config_url: str) -> Optional[str]:
        logger.info(f"Fetching config from '{rule_config_url}'")
        try:
            r = requests.get(
                rule_config_url,
                headers={"User-Agent": SEMGREP_USER_AGENT},
                timeout=60,
            )
            r.raise_for_status()
        except requests.Timeout:
            logger.warning(
                f"There was a timeout error trying to download the rule config at '{rule_config_url}'"
            )
            return None
        except requests.HTTPError as e:
            logger.warning(
                f"There was an issue connecting to '{rule_config_url}'; error: {e}"
            )
            return None
        return r.text

    def _fetch_rule_config_from_path(self, path: Path) -> Optional[str]:
        pass

    def _write(self, path: Path, text: Optional[str]) -> None:
        if not text:
            raise ValueError(
                f"Data was not received from '{self.config_str}'. Could not write config"
            )

        logger.info(f"Rule config '{self.config_str}' has been written to '{path}'")
        path.write_text(text)

    def is_path(self) -> bool:
        return Path(self.config_str).expanduser().resolve().exists()

    def is_url(self) -> bool:
        parsed = urlparse(self.config_str)
        return parsed.scheme != "" and parsed.netloc != ""

    def is_short_config(self) -> bool:
        """
        Return true if the second character is a forward slash, such
        as in 'p/r2c' or 'r/python'
        """
        return self.config_str.find("/") == 1

    def normalize_rule_config_name(self) -> str:
        return self.config_str.replace("/", ".") + ".yaml"

    def resolve_to_cache(self, cache_path: Path) -> None:
        """
        Resolves this RuleConfig to a path on the filesystem.
        If the rule config string is already a path on the system, just use that path.
        If the rule config string is a URL, download the rules and add that to the supplied cache path.
        If the rule config string is a short config like 'p/r2c', download the rules and add that to the supplied cache path.
        Raises a ValueError if it cannot resolve the config string.
        """
        if self.is_path():
            import shutil

            logger.info(
                f"Config '{self.config_str}' is already on filesystem. Copying to cache at '{cache_path}'"
            )
            shutil.copytree(Path(self.config_str).expanduser().resolve(), cache_path)
        elif self.is_url():
            consolidated_rules = self._fetch_rule_config_from_url(self.config_str)
            self._write(cache_path, consolidated_rules)
        elif self.is_short_config():
            rule_config_url = f"{SEMGREP_URL}/{self.config_str}"
            consolidated_rules = self._fetch_rule_config_from_url(rule_config_url)
            self._write(cache_path, consolidated_rules)

        raise ValueError(
            f"Could not resolve location of the config string '{self.config_str}'. Try using a filesystem path or valid Semgrep config"
        )


@attr.s
class BenchmarkRunSetupData(object):
    """
    Stores data about an individual benchmark run
    """

    run_name: str = attr.ib(default="benchmark_run")
    rule_configs: List[RuleConfig] = attr.ib(default=["p/r2c"])
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
    def parse_config(cls, config_file: Path) -> SemgrepBenchmarkConfig:
        logger.debug(f"Using config at {config_file.absolute()}")
        with open(config_file, "r") as fin:
            config = yaml.load(fin)

        return SemgrepBenchmarkConfig(
            [
                BenchmarkRunSetupData(
                    run_name=config_data.get("name", f"run-{int(time.time())}"),
                    rule_configs=[
                        RuleConfig(config_str)
                        for config_str in config_data.get("rule_configs", [])
                    ],
                    repositories=[
                        Repository(repo.get("url"), repo.get("commit_hash"))
                        for repo in config_data.get("repos", [])
                    ],
                    semgrep_options=config_data.get("opts", []),
                )
                for config_data in config.get("runs", [])
            ]
        )
