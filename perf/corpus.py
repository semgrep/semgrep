import subprocess
from pathlib import Path
from typing import Optional
from typing import Union

# Run command and propagate errors
def cmd(*args: str) -> None:
    subprocess.run(args, check=True)  # nosem


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


SEMGREP_CORE_CORPUSES = [
    # Run Ajin's nodejsscan rules on some repo containing javascript files.
    # This takes something like 4 hours or more. Maybe we could run it
    # on fewer targets.
    # Corpus("njs", "input/njsscan/njsscan/rules/semantic_grep", "input/juice-shop"),
    Corpus("big-js", "input/semgrep.yml", "input/big-js", "js"),
    # Commented out because it can't run with semgrep-core
    # Corpus(
    #    "njsbox", "input/njsscan/njsscan/rules/semantic_grep", "input/dropbox-sdk-js", "js"
    # ),
    Corpus("zulip", "input/semgrep.yml", "input/zulip", "python"),
    # The tests below all run r2c rulepacks (in r2c-rules) on public repos
    # For command Corpus("$X", ..., "input/$Y"), you can find the repo by
    # going to github.com/$X/$Y
    #
    # Run our django rulepack on a large python repo
    Corpus("apache", "input/django.yml", "input/libcloud", "python"),
    # Run our flask rulepack on a python repo
    Corpus("dropbox", "input/flask.yml", "input/pytest-flakefinder", "python"),
    # Run our golang rulepack on a go/html repo
    Corpus("0c34", "input/golang.yml", "input/govwa", "go"),
    # Run our ruby rulepack on a large ruby repo
    Corpus("rails", "input/ruby.yml", "input/rails", "ruby"),
    # Also commented out because it can't run with semgrep-core
    # Run our javascript and eslint-plugin-security packs on a large JS repo
    # Corpus("lodash", "input/rules", "input/lodash", "js"),
]


# Naming conventions:
#
# Save for a few repos (currently zulip, big-js, njsbox),
# the tests below all run r2c rulepacks (in r2c-rules) on public repos
# For command Corpus("$X", ..., "input/$Y"), you can find the repo by
# going to github.com/$X/$Y
#

SMALL_CORPUSES = [
    # Run zulip custom Python rules on zulip itself
    Corpus("zulip", "input/semgrep.yml", "input/zulip"),
    # Run our flask rulepack on a python repo
    Corpus("dropbox", "input/flask.yml", "input/pytest-flakefinder"),
    # Run our r2c-ci and r2c-security-audit packs on a go/ruby repo
    Corpus("coinbase", "input/rules", "input/bifrost"),
    # Run our django rulepack on a large python repo
    Corpus("apache", "input/django.yml", "input/libcloud"),
    # Run our golang rulepack on a go/html repo
    Corpus("0c34", "input/golang.yml", "input/govwa"),
    # Run our javascript and eslint-plugin-security packs on a large JS repo
    Corpus("lodash", "input/rules", "input/lodash"),
    # Run old nodejsscan rules on vulnerable app (was in calculate_ci_perf.py)
    Corpus("njs-old-dvna", "input/njsscan/njsscan/rules/semantic_grep", "input/dvna"),
    #
    # Run our r2c-ci packs (but not r2c-security-audit) on vulnerable apps
    # See https://owasp.org/www-project-vulnerable-web-applications-directory/
    # for a full list of such apps
    #
    Corpus("DVWA", "input/rules", "input/DVWA"),
    Corpus("juice-shop", "input/rules", "input/juice-shop"),
    Corpus("Vulnerable-Flask-App", "input/rules", "input/Vulnerable-Flask-App"),
    # (Gitlab small) Run our python and flask rules on a python repo
    Corpus("pallets", "input/rules", "input/flask"),
    # (Gitlab small) Run our javascript rules on a JS repo
    Corpus("socketio", "input/javascript.yml", "input/socket"),
    #
    # This is for more comprehensive rule timing information
    #
    # small java corpus
    Corpus("coolMenu", "input/java.yml", "input/coolMenu"),
    # small c corpus
    Corpus("t00sh", "input/rules", "input/rop-tool"),
    # small repository of dockerfiles
    Corpus("grpc", "input/docker.yml", "input/grpc-docker-library"),
]

MEDIUM_CORPUSES = [
    # Single rule bench at the origin of the --filter-irrelevant-rules opti
    Corpus("big-js", "input/semgrep.yml", "input/big-js"),
    # Some nodejsscan bench
    Corpus(
        "njs-box", "input/njsscan/njsscan/rules/semantic_grep", "input/dropbox-sdk-js"
    ),
    # Run old nodejsscan rules on vulnerable app (was in calculate_ci_perf.py)
    Corpus(
        "njs-old-juice", "input/njsscan/njsscan/rules/semantic_grep", "input/juice-shop"
    ),
    # Run our r2c-ci and r2c-security audit packs on a python/JS repo
    Corpus("netflix", "input/rules", "input/lemur"),
    # Run our r2c-ci and r2c-security audit packs on a JS/other repo
    Corpus("draios", "input/rules", "input/sysdig-inspect"),
    # (Gitlab medium) Run our python and flask packs on a python repo
    Corpus("django", "input/rules", "input/django"),
    # (Gitlab medium) Run our r2c-ci and r2c-security audit packs on a java repo
    Corpus("dropwizard", "input/rules", "input/dropwizard"),
    # (Gitlab medium) Run our r2c-ci and r2c-security audit packs on a java repo
    Corpus("pmd", "input/rules", "input/pmd"),
    # (Gitlab large) Run our security-audit pack on a c repo
    Corpus("smacker", "input/r2c-security-audit.yml", "input/gotree"),
    # (Gitlab large) Run our java pack on a java repo
    Corpus("spring-projects", "input/java.yml", "input/spring"),
    # Run our ruby rulepack on a large ruby repo
    Corpus("rails", "input/ruby.yml", "input/rails"),
]

# By default, these will not run
LARGE_CORPUSES = [
    #
    # Run Ajin's nodejsscan rules on some repo containing javascript files.
    # This takes something like 4 hours or more. Maybe we could run it
    # on fewer targets.
    #
    Corpus(
        "njs-juice", "input/njsscan/njsscan/rules/semantic_grep", "input/juice-shop"
    ),
    # (Gitlab large) Run our javascript and r2c-security audit packs on a js/ruby repo
    Corpus("gitlab", "input/rules", "input/gitlab"),
]

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
