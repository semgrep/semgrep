from semgrep.constants import SEMGREP_URL


def is_registry_rule(rule: str) -> bool:
    return (
        rule.startswith(SEMGREP_URL + "/p/")
        or rule.startswith("p/")
        or rule.startswith(SEMGREP_URL + "/r/")
        or rule.startswith("r/")
        or rule == "auto"
    )


# TODO support auto. this should happen automatically when starting
# to use config_resolver
def url_for_rule(rule: str) -> str:
    if rule.startswith(SEMGREP_URL + "/p/"):
        return rule.replace(SEMGREP_URL + "/p/", SEMGREP_URL + "/c/p/")

    if rule.startswith(SEMGREP_URL + "/r/"):
        return rule.replace(SEMGREP_URL + "/r/", SEMGREP_URL + "/c/r/")

    if rule.startswith(SEMGREP_URL + "/c/"):
        return rule

    return SEMGREP_URL + "/c/" + rule
