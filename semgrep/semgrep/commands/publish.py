import sys
from enum import Enum
from pathlib import Path
from typing import Any
from typing import Dict
from typing import List
from typing import Optional
from typing import Tuple

import click

from semgrep.commands.login import Authentication
from semgrep.config_resolver import get_config
from semgrep.constants import SEMGREP_URL
from semgrep.project import get_project_url
from semgrep.test import get_config_filenames
from semgrep.test import get_config_test_filenames
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

SEMGREP_REGISTRY_BASE_URL = SEMGREP_URL
SEMGREP_REGISTRY_UPLOAD_URL = f"{SEMGREP_REGISTRY_BASE_URL}api/registry/rule"
SEMGREP_REGISTRY_VIEW_URL = f"{SEMGREP_REGISTRY_BASE_URL}r/"
SEMGREP_SNIPPET_VIEW_URL = f"{SEMGREP_REGISTRY_BASE_URL}s/"


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


def _get_test_code_for_config(
    target: Path,
) -> Tuple[List[Path], Dict[Path, List[Path]]]:
    config = target
    config_filenames = get_config_filenames(config)
    if len(config_filenames) == 0:
        config_filenames = [target]
    config_test_filenames = get_config_test_filenames(config, config_filenames, target)
    return config_filenames, config_test_filenames


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
    " If 'public', rules will be published to the Semgrep Registry (requires --registry-id)",
)
@click.option(
    "--registry-id",
    "registry_id",
    help="If --visibility is set to public, this is the path the rule will have in the registry (example: python.flask.my-new-rule",
)
def publish(
    target: str, visibility: VisibilityState, registry_id: Optional[str]
) -> None:
    """
    If logged in, uploads a rule to the Semgrep Registry with the specified visibility.

    Public rules need registry_id to specify where in the public registry they should live.
    """
    saved_login_token = Authentication.get_token()
    fail_count = 0
    if saved_login_token:
        config_filenames, config_test_filenames = _get_test_code_for_config(
            Path(target)
        )
        if len(config_filenames) == 0:
            click.echo(f"No valid Semgrep rules found in {target}", err=True)
            sys.exit(1)
        if len(config_filenames) != 1 and visibility == VisibilityState.PUBLIC:
            click.echo(
                f"Only one public rule can be uploaded at a time: specify a single Semgrep rule",
                err=True,
            )
            sys.exit(1)
        if visibility == VisibilityState.PUBLIC and registry_id is None:
            click.echo(f"--visibility=public requires --registry-id", err=True)
            sys.exit(1)

        click.echo(
            f'Found {len(config_filenames)} configs to publish with visibility "{visibility}"'
        )

        for config_filename in config_filenames:
            test_cases = config_test_filenames.get(config_filename, [])
            click.echo(
                f"--> Uploading {config_filename} (test cases: {[str(t) for t in test_cases]})"
            )
            first_test_case = test_cases[0] if len(test_cases) >= 1 else None

            if not _upload_rule(
                config_filename,
                saved_login_token,
                visibility,
                first_test_case,
                registry_id,
            ):
                fail_count += 1
        if fail_count == 0:
            sys.exit(0)
        else:
            click.echo(f"{fail_count} rules failed to upload", err=True)
            sys.exit(1)
    else:
        click.echo("run `semgrep login` before using upload", err=True)
        sys.exit(1)


def _upload_rule(
    rule_file: Path,
    token: str,
    visibility: VisibilityState,
    test_code_file: Optional[Path],
    registry_id: Optional[str],
) -> bool:
    """
    Uploads rule in rule_file with the specificied visibility
    Args:
        rule_file: path to valid rule yaml file that contains single
        rule to be uploaded
        token: token with permissions to upload a rule
        visibility: the visibility of the uploaded rule
        test_code_file: optional test case to attach with the rule
    """
    config, errors = get_config(None, None, [str(rule_file)], project_url=None)

    if errors:
        click.echo(
            f"    Invalid rule definition: {str(rule_file)} is invalid: {errors}",
            err=True,
        )
        return False

    rules = config.get_rules(True)
    if len(rules) != 1:
        click.echo(
            "    Rule contains more than one rule: only yaml files with a single can be published",
            err=True,
        )
        return False

    rule = rules[0]

    # add metadata about the origin of the rule
    rule.metadata[
        "rule-origin-note"
    ] = f"published from {rule_file} in {get_project_url()}"

    import requests

    session = requests.Session()
    session.headers["Authorization"] = f"Bearer {token}"

    request_json = {
        "definition": {"rules": [rule._raw]},
        "visibility": visibility,
        # below should always be defined if passed validation
        "language": rule.languages[0] if len(rule.languages) >= 1 else None,
        # TODO backend can infer deployment ID from token; shouldn't need this
        "deployment_id": Authentication.get_deployment_id(),
        "test_target": test_code_file.read_text() if test_code_file else None,
        "registry_check_id": registry_id,
    }

    response = session.post(SEMGREP_REGISTRY_UPLOAD_URL, json=request_json, timeout=30)

    if not response.ok:
        click.echo(
            f"    Failed to upload rule with status_code {response.status_code}",
            err=True,
        )
        click.echo(response.text, err=True)
        return False
    else:
        created_rule = response.json()

        if visibility == VisibilityState.PUBLIC:
            click.echo(
                f"    Pull request created for this public rule at: {created_rule['pr_url']}"
            )
        elif visibility == VisibilityState.UNLISTED:
            click.echo(
                f"    Published {visibility} rule at {SEMGREP_SNIPPET_VIEW_URL}{created_rule['id']}"
            )
        else:
            click.echo(
                f"    Published {visibility} rule at {SEMGREP_REGISTRY_VIEW_URL}{created_rule['path']}"
            )

    return True
