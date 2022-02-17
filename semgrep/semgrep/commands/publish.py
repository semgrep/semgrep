import os
import sys
from enum import Enum
from typing import Any
from typing import Optional

import click

from semgrep.commands.login import Authentication
from semgrep.config_resolver import get_config
from semgrep.project import get_project_url
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

SEMGREP_REGISTRY_BASE_URL = os.environ.get(
    "SEMGREP_REGISTRY_BASE_URL", "https://semgrep.dev"
)
SEMGREP_REGISTRY_UPLOAD_URL = f"{SEMGREP_REGISTRY_BASE_URL}/api/registry/rule"
SEMGREP_REGISTRY_VIEW_URL = f"{SEMGREP_REGISTRY_BASE_URL}/r/"


class VisibilityState(str, Enum):
    ORG_PRIVATE: str = "org_private"
    UNLISTED: str = "unlisted"
    PUBLIC: str = "public"


class VisibilityStateType(click.ParamType):
    name = "visibility_state"

    def get_metavar(self, param: click.Parameter) -> str:
        return "[org_private|unlisted|public]"

    def convert(
        self,
        value: Any,
        param: Optional["click.Parameter"],
        ctx: Optional["click.Context"],
    ) -> Any:
        if value is None:
            return None
        if isinstance(value, str):
            lower = value.lower()
            if lower == "org_private":
                return VisibilityState.ORG_PRIVATE
            if lower == "unlisted":
                return VisibilityState.UNLISTED
            if lower == "public":
                return VisibilityState.PUBLIC
        self.fail("expected 'org_private', 'unlisted', or 'public'")


VISIBILITY_STATE = VisibilityStateType()


@click.command()
@click.argument("target", nargs=1, type=click.Path(allow_dash=True))
@click.option(
    "--visibility",
    "visibility",
    default="org_private",
    type=VISIBILITY_STATE,
    help="Sets visibility of the uploaded rules."
    " If 'org_private', rules will be private to your org (default)"
    " If 'unlisted', rules will be listed in your org, but not listed to non-org members"
    " If 'public', rules will be published to the Semgrep Registry",
)
def publish(target: str, visibility: str) -> None:
    """
    If logged in, uploads a private rule to the Semgrep registry.

    If not logged in, explains how to make a PR to semgrep-rules.
    """
    saved_login_token = Authentication.read_token()
    if saved_login_token:
        if _upload_rule(target, saved_login_token, visibility):
            sys.exit(0)
        else:
            sys.exit(1)

    else:
        click.echo("run `semgrep login` before using upload", err=True)


def _publish_rule_to_registry(rule_file: str) -> bool:
    # TODO: clone semgrep-rules, checkout a fresh branch, update/overwrite the target rule, then suggest making a PR?
    pass


def _upload_rule(rule_file: str, token: str, visibility: str) -> bool:
    """
    Uploads rule in rule_file to private registry of deployment_id
    Args:
        rule_file: path to valid rule yaml file that contains single
        rule to be uploaded
        deployment_id: which deployment to upload a rule to
        token: token with permissions to upload a rule
    """
    config, errors = get_config(None, None, [rule_file], project_url=None)

    if errors:
        click.echo(f"Rule definition: {str(rule_file)} is invalid: {errors}", err=True)
        return False

    rules = config.get_rules(True)
    if len(rules) != 1:
        click.echo(
            "Rule contains more than one rule: only yaml files with a single can be published",
            err=True,
        )
        return False

    rule = rules[0]

    # add metadata about the origin of the rule
    rule.metadata[
        "source-rule-url"
    ] = f"published from {rule_file} in {get_project_url()}"

    import requests

    session = requests.Session()
    session.headers["Authorization"] = f"Bearer {token}"

    request_json = {
        "definition": {"rules": [rule._raw]},
        "visibility": visibility,
        # below should always be defined if passed validation
        "languages": rule.languages,
        # TODO backend can infer deployment ID from token; shouldn't need this
        "deployment_id": Authentication.get_deployment_id(),
    }

    response = session.post(SEMGREP_REGISTRY_UPLOAD_URL, json=request_json, timeout=30)

    if not response.ok:
        click.echo(
            f"Failed to upload rule with status_code {response.status_code}", err=True
        )
        click.echo(response.text, err=True)
        return False
    else:
        created_rule = response.json()
        click.echo(
            f"You can find your {visibility} rule at {SEMGREP_REGISTRY_VIEW_URL}{created_rule['path']}"
        )

    return True
