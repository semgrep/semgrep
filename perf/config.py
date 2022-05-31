import logging
import os
import shutil
import sys
import time
from pathlib import Path
from typing import List
from typing import Optional
from typing import Type
from urllib.parse import urlparse

import requests
from attrs import define
from attrs import field
from constants import SEMGREP_URL
from ruamel.yaml import YAML


logger = logging.getLogger(__file__)
logger.setLevel(logging.DEBUG)
handler = logging.StreamHandler(stream=sys.stderr)
handler.setFormatter(
    logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
)
logger.addHandler(handler)

yaml = YAML(typ="rt")


@define
class Repository:
    url: str = field(default="")
    commit_hash: str = field(default="HEAD")


@define
class RuleConfig:
    config_str: str = field(default="")

    def _fetch_rule_config_from_url(self, rule_config_url: str) -> Optional[str]:
        logger.info(f"Fetching config from '{rule_config_url}'")
        try:
            r = requests.get(
                rule_config_url, headers={"User-Agent": "perf.config"}, timeout=60
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

    def _fetch_njsscan_rules(self, cache_path: Path) -> None:
        logger.debug("Getting rules from the njsscan repo")

        cur_dir = os.getcwd()
        repo = "https://github.com/ajinabraham/njsscan.git"
        commit_id = "edc1d0a8b38bfb5421e21665284f83de3dbe636e"
        njs_tempdir = "njsscan-temp"
        njs_subdir = f"{njs_tempdir}/njsscan/rules/semantic_grep"

        # Clone njsscan and move relevant rules to njsscan, then remove rest of repo
        os.mkdir(cache_path / njs_tempdir)
        os.chdir(cache_path / njs_tempdir)
        os.system("git init")
        os.system(f"git remote add origin {repo}")
        os.system(f"git fetch --depth 1 origin ${commit_id}")
        os.system("git checkout FETCH_HEAD -b tmp")
        os.chdir("..")
        os.replace(njs_subdir, "njsscan")
        shutil.rmtree(njs_tempdir)

        logger.debug("Done getting rules from njsscan repo")
        os.chdir(cur_dir)

    def is_path(self) -> bool:
        logger.debug(
            f"Checking if config string is a path. Resolved path is {Path(self.config_str).expanduser().resolve()}"
        )
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

    def is_njsscan(self) -> bool:
        """
        Special case: Return true if the str is njsscan
        """
        return self.config_str == "njsscan"

    def normalize_rule_config_name(self) -> str:
        if self.is_njsscan():
            return self.config_str
        return (self.config_str.replace("/", ".") + ".yaml").strip(".")

    def resolve_to_cache(self, cache_path: Path) -> None:
        """
        Resolves this RuleConfig to a path on the filesystem.
        If the rule config string is already a path on the system, just use that path.
        If the rule config string is a URL, download the rules and add that to the supplied cache path.
        If the rule config string is a short config like 'p/r2c', download the rules and add that to the supplied cache path.
        Raises a ValueError if it cannot resolve the config string.
        """
        if (
            self.is_njsscan()
        ):  # TODO make this flexible, don't rely on hardcoding njsscan
            import os  # I don't know why this is necessary

            os.mkdir(cache_path)
            self._fetch_njsscan_rules(cache_path)
        elif self.is_path():
            import os
            import shutil

            logger.info(
                f"Config '{self.config_str}' is already on filesystem. Copying to cache at '{cache_path}'"
            )
            config_path = Path(self.config_str).expanduser().resolve()
            if config_path.is_dir():
                logger.debug(f"Copying directory from {config_path} to {cache_path}")
                if cache_path.exists():
                    shutil.rmtree(cache_path)
                shutil.copytree(config_path, cache_path)
            elif config_path.is_file():
                logger.debug(f"Copying rules file from {config_path} to {cache_path}")
                if cache_path.exists():
                    os.remove(cache_path)
                shutil.copyfile(config_path, cache_path)
            else:
                raise ValueError(
                    f"Config string {self.config_str} exists on filesystem but is not a file or directory."
                )
        elif self.is_url():
            consolidated_rules = self._fetch_rule_config_from_url(self.config_str)
            self._write(cache_path, consolidated_rules)
        elif self.is_short_config():
            rule_config_url = f"{SEMGREP_URL}/{self.config_str}"
            consolidated_rules = self._fetch_rule_config_from_url(rule_config_url)
            self._write(cache_path, consolidated_rules)
        else:
            raise ValueError(
                f"Could not resolve location of the config string '{self.config_str}'. Try using a filesystem path or valid Semgrep config"
            )
        return cache_path


@define
class BenchmarkRunSetupData:
    """
    Stores data about an individual benchmark run
    """

    run_name: str = field(default="benchmark_run")
    rule_configs: List[RuleConfig] = field(default=["p/r2c"])
    repositories: List[Repository] = field(factory=list)
    semgrep_options: List[str] = field(factory=list)


@define
class SemgrepBenchmarkConfig:
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

    benchmark_setup_data: List[BenchmarkRunSetupData] = field(factory=list)

    @classmethod
    def parse_config(
        cls: Type["SemgrepBenchmarkConfig"], config_file: Path
    ) -> "SemgrepBenchmarkConfig":
        logger.debug(f"Using config at {config_file.absolute()}")
        with open(config_file) as fin:
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
