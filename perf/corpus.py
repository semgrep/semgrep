import subprocess
from pathlib import Path
from typing import Optional
from typing import Union

# Run command and propagate errors
def cmd(*args: str) -> None:
    subprocess.run(args, check=True)  # nosem


CORPUS_LOCATION = "bench"


class Corpus:
    def __init__(
        self,
        name: str,
        rule_dir: Union[str, Path],
        target_dir: Union[str, Path],
        language: Optional[str] = None,
    ):
        # name for the input corpus (rules and targets)
        self.name = name

        # folder containing the semgrep rules
        self.rule_dir = rule_dir

        # folder containing the target source files
        self.target_dir = target_dir

        # language to run rules with (because semgrep-core requires it)
        self.language = language

    # Fetch rules and targets is delegated to an ad-hoc script named 'prep'.
    def prep(self) -> None:
        cmd("./prep")


SMALL_CORPUSES = Path("configs/ci_small_repos.yaml")
MEDIUM_CORPUSES = Path("configs/ci_medium_repos.yaml")
LARGE_CORPUSES = Path("configs/ci_large_repos.yaml")

# For corpuses that cannot be run in CI because they use private repos
INTERNAL_CORPUSES = [
    Corpus("dogfood", "input/semgrep.yml", "input/"),
]

# Due to the semgrep-gitlab partnership, we want to benchmark the
# speed of the rules gitlab runs on repos they have suggested
GITLAB_CORPUSES = [
    # Semgrep-app
    Corpus("semgrep-app", "../gitlab-rules", "input/semgrep-app"),
    # Gitlab small
    Corpus("pallets", "../gitlab-rules", "input/flask"),
    # Gitlab small
    Corpus("socketio", "../gitlab-rules", "input/socket"),
    # (Gitlab medium) python repo
    Corpus("django", "../gitlab-rules", "input/django"),
    # (Gitlab medium) java repo
    Corpus("dropwizard", "../gitlab-rules", "input/dropwizard"),
    # (Gitlab large) js/ruby repo
    Corpus("gitlab", "../gitlab-rules", "input/gitlab"),
    # (Gitlab large) c repo
    Corpus("smacker", "../gitlab-rules", "input/gotree"),
    # (Gitlab large) java repo
    Corpus("spring-projects", "../gitlab-rules", "input/spring"),
    # (Gitlab medium) java repo
    Corpus("pmd", "../gitlab-rules", "input/pmd"),
]

DUMMY_CORPUSES = [Corpus("dummy", "input/dummy/rules", "input/dummy/targets", "js")]
