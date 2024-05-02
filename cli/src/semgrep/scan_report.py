##############################################################################
# Prelude
##############################################################################
# Helpers to run_scan.py to report scan status
import sys
from textwrap import wrap
from typing import List
from typing import Sequence

from attrs import evolve
from rich.columns import Columns
from rich.padding import Padding
from rich.table import Table

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.app import auth
from semgrep.console import console
from semgrep.console import Title
from semgrep.constants import Colors
from semgrep.core_runner import CoreRunner
from semgrep.core_runner import Plan
from semgrep.rule import Rule
from semgrep.state import DesignTreatment
from semgrep.state import get_state
from semgrep.target_manager import TargetManager
from semgrep.target_mode import TargetModeConfig
from semgrep.util import unit_str
from semgrep.util import with_color
from semgrep.util import with_feature_status

##############################################################################
# Helpers
##############################################################################


# TODO: Use an array of semgrep_output_v1.Product instead of booleans flags for secrets, code, and supply chain
def _print_product_status(sast_enabled: bool = True, sca_enabled: bool = False) -> None:
    """
    (Simple) print the statuses of enabled products to stdout when the user
    is given the product-focused CLI UX treatment.
    """
    learn_more_url = with_color(Colors.cyan, "https://sg.run/cloud", underline=True)
    login_command = with_color(Colors.gray, "`semgrep login`")
    is_logged_in = auth.get_token() is not None
    all_enabled = True  # assume all enabled until we find a disabled product

    sections = [
        (
            "Semgrep OSS",
            True,
            [
                "Basic security coverage for first-party code vulnerabilities.",
            ],
        ),
        (
            "Semgrep Code (SAST)",
            is_logged_in and sast_enabled,
            [
                "Find and fix vulnerabilities in the code you write with advanced scanning and expert security rules.",
            ],
        ),
        (
            "Semgrep Supply Chain (SCA)",
            sca_enabled,
            [
                "Find and fix the reachable vulnerabilities in your OSS dependencies.",
            ],
        ),
    ]

    for name, enabled, features in sections:
        all_enabled = all_enabled and enabled
        console.print(
            f"\n{with_feature_status(enabled=enabled)} {with_color(Colors.foreground, name, bold=True)}"
        )
        for feature in features:
            console.print(f"  {with_feature_status(enabled=enabled)} {feature}")

    if not is_logged_in:
        message = "\n".join(
            wrap(
                f"💎 Get started with all Semgrep products via {login_command}.",
                width=80,
            )
            + [f"✨ Learn more at {learn_more_url}."]
        )
        console.print(f"\n{message}\n")
    elif not all_enabled:
        # TODO: Handle cases where SAST / SCA are not enabled (GROW-53)
        # We should suggest a resolution such as enabling supply chain in SCP settings, and
        # run then `semgrep ci`. However, there are some additional edge cases to consider
        # such as feature availability / required plan upgrades, and thus we will punt showing
        # a resolution for now.
        console.print(" ")  # space intentional for progress bar padding
    else:
        console.print(" ")  # space intentional for progress bar padding


def _print_scan_plan_header(
    target_manager: TargetManager,
    sast_plan: Plan,
    sca_plan: Plan,
    target_mode_config: TargetModeConfig,
    cli_ux: DesignTreatment = DesignTreatment.LEGACY,
) -> None:
    """
    Prints the number of files intended to be scanned and (optionally)
    the number of rules to be run based on the current configuration.
    """
    file_count = len(target_manager.get_all_files())
    legacy_cli_ux = cli_ux == DesignTreatment.LEGACY
    simple_ux = cli_ux == DesignTreatment.SIMPLE

    if target_mode_config.is_pro_diff_scan:
        total_file_count = len(
            evolve(target_manager, baseline_handler=None).get_all_files()
        )
        diff_file_count = len(target_mode_config.get_diff_targets())
        summary_line = (
            f"Inter-file Differential Scanning {unit_str(diff_file_count, 'file')}"
        )
    else:
        file_count = len(target_manager.get_all_files())
        summary_line = f"Scanning {unit_str(file_count, 'file')}"

    if target_manager.respect_git_ignore:
        summary_line += (
            f" {'tracked by git' if legacy_cli_ux else '(only git-tracked)'}"
        )

    if simple_ux:  # We skip printing the rule count with new simple CLI UX
        console.print(f"{summary_line} with:")
        return

    # The sast_plan contains secrets rules too.  You might be tempted
    # to use rule_count_by_product but the summary line doesn't
    # historically take into account the effects of not scanning
    # files, which rule_count_by_product includes.
    sast_rule_count = len(sast_plan.rules)
    is_secret_rule = lambda r: isinstance(r.product.value, out.Secrets)
    secrets_rule_count = len(list(filter(is_secret_rule, sast_plan.rules)))

    # TODO code_rule_count currently double counts pro_rules.
    code_rule_count = sast_rule_count - secrets_rule_count
    summary_line += f" with {unit_str(code_rule_count, 'Code rule')}"

    if secrets_rule_count:
        summary_line += f", {unit_str(secrets_rule_count, 'Secrets rule')}"

    sca_rule_count = len(sca_plan.rules)
    if sca_rule_count:
        summary_line += f", {unit_str(sca_rule_count, 'Supply Chain rule')}"

    console.print(summary_line + ":")


def _print_tables(tables: List[Table]) -> None:
    columns = Columns(tables, padding=(1, 8))

    # rich tables are 2 spaces indented by default
    # deindent only by 1 to align the content, instead of the invisible table border
    console.print(Padding(columns, (1, 0)), deindent=1)


def _print_degenerate_table(plan: Plan, *, rule_count: int) -> None:
    """
    Print a table with no rows and a simple message instead.
    """
    if not rule_count or not plan.target_mappings:
        console.print("Nothing to scan.")
    else:  # e.g. 1 rule, 4 files
        console.print(f"Scanning {unit_str(len(plan.target_mappings), 'file')}.")


def _print_sast_table(
    sast_plan: Plan, *, product: out.Product, rule_count: int
) -> None:
    """
    Pretty print the SAST / secrets plan to stdout.
    """
    if rule_count <= 1 or not sast_plan.target_mappings:
        _print_degenerate_table(sast_plan, rule_count=rule_count)
        return

    plan_by_lang = sast_plan.split_by_lang_label_for_product(product)

    if len(plan_by_lang) == 1:
        [(language, target_mapping)] = plan_by_lang.items()
        console.print(
            f"Scanning {unit_str(target_mapping.file_count, 'file')} with {unit_str(rule_count, f'{language} rule')}."
        )
        return

    use_color = sys.stderr.isatty()
    # NOTE: osemgrep lacks support for bold table headers
    # Given that this is only printed in legacy and compatability
    # runs, the output doesn't really matter
    _print_tables(
        [
            sast_plan.table_by_language(with_tables_for=product, use_color=use_color),
            sast_plan.table_by_origin(with_tables_for=product, use_color=use_color),
        ]
    )


def _print_sca_table(sca_plan: Plan, rule_count: int) -> None:
    """
    Pretty print the sca plan to stdout with the legacy CLI UX.
    """
    if rule_count <= 1 or not sca_plan.target_mappings:
        _print_degenerate_table(sca_plan, rule_count=rule_count)
        return

    _print_tables([sca_plan.table_by_ecosystem()])
    console.print("\n")  # space intentional to force second table to be on its own line
    _print_tables([sca_plan.table_by_sca_analysis()])


def _print_detailed_sca_table(
    sca_plan: Plan, rule_count: int, with_supply_chain: bool = False
) -> None:
    """
    Pretty print the plan to stdout with the detailed CLI UX.
    """
    if rule_count:
        _print_sca_table(sca_plan, rule_count)
        return

    sep = "\n   "
    message = "No rules to run."
    """
    We need to account for several edges cases:
        - `semgrep ci` was invoked but no rules were found (e.g. no lockfile).
        - `semgrep scan` was invoked with the supply-chain flag and no rules found.
        - `semgrep ci` was invoked without the supply-chain flag or feature enabled.
    """
    # 1. Validate that the user is indeed running SCA (and not from semgrep scan).
    is_scan = get_state().is_scan_invocation()
    # 2. Check if the user has metrics enabled.
    metrics = get_state().metrics
    metrics_enabled = metrics.is_enabled
    # If the user has metrics enabled, we can suggest they run `semgrep ci` to get more findings.
    # Otherwise, we should expect the user to be already aware of the other products.
    # 3. Check if the user has logged in.
    has_auth = auth.get_token() is not None
    # Users who have not logged in will not be able to run `semgrep ci`.
    # For users with metrics enabled who are running scan without auth,
    # we should suggest they login and run semgrep ci.
    if is_scan and metrics_enabled:
        login_command = with_color(Colors.gray, "`semgrep login`")
        ci_command = with_color(Colors.gray, "`semgrep ci`")
        if not has_auth:
            message = sep.join(
                wrap(
                    f"💎 Sign in with {login_command} and run {ci_command} to find dependency vulnerabilities and advanced cross-file findings.",
                    width=70,
                )
            )
        elif not with_supply_chain:
            message = sep.join(
                wrap(
                    f"💎 Run {ci_command} to find dependency vulnerabilities and advanced cross-file findings.",
                    width=70,
                )
            )
        else:  # supply chain but no rules (e.g. no lockfile)
            pass
    else:  # skip nudge for users who have not enabled metrics or are already running ci
        pass
    console.print(f"\n{message}\n")


##############################################################################
# Entry point
##############################################################################


def print_scan_status(
    rules: Sequence[Rule],
    target_manager: TargetManager,
    target_mode_config: TargetModeConfig,
    *,
    cli_ux: DesignTreatment = DesignTreatment.LEGACY,
    # TODO: Use an array of semgrep_output_v1.Product instead of booleans flags for secrets, code, and supply chain
    with_code_rules: bool = True,
    with_supply_chain: bool = False,
) -> List[Plan]:
    """
    Prints the scan status and returns the plans
    """
    legacy_ux = cli_ux == DesignTreatment.LEGACY
    simple_ux = cli_ux == DesignTreatment.SIMPLE
    detailed_ux = cli_ux == DesignTreatment.DETAILED
    minimal_ux = cli_ux == DesignTreatment.MINIMAL

    sast_plan = CoreRunner.plan_core_run(
        [
            rule
            for rule in rules
            if (
                (
                    isinstance(rule.product.value, out.SAST)
                    or isinstance(rule.product.value, out.Secrets)
                )
                and (not rule.from_transient_scan)
            )
        ],
        target_manager
        if not target_mode_config.is_pro_diff_scan
        else evolve(
            target_manager, target_strings=target_mode_config.get_diff_targets()
        ),
        product=out.Product(
            out.SAST()
        ),  # code-smell since secrets and sast are within the same plan
    )

    lockfiles = target_manager.get_all_lockfiles()
    sca_plan = CoreRunner.plan_core_run(
        [
            rule
            for rule in rules
            if isinstance(rule.product.value, out.SCA)
            and any(lockfiles[ecosystem] for ecosystem in rule.ecosystems)
        ],
        target_manager,
        product=out.Product(out.SCA()),
    )

    plans = [sast_plan, sca_plan]

    # We skip printing the remaining output for pattern invocations
    if minimal_ux:
        return plans

    if simple_ux:
        logo = with_color(Colors.green, "○○○")
        console.print(
            f"""
┌──── {logo} ────┐
│ Semgrep CLI │
└─────────────┘
"""
        )
    else:
        console.print(Title("Scan Status"))

    _print_scan_plan_header(
        target_manager, sast_plan, sca_plan, target_mode_config, cli_ux
    )

    sca_rule_count = len(sca_plan.rules)
    has_sca_rules = sca_rule_count > 0

    # NOTE: There's some funky behavior with handling the rule counts
    # in which some functions require rule counts calculated in different ways.
    alt_sast_rule_count = sast_plan.rule_count_for_product(out.Product(out.SAST()))
    alt_sca_rule_count = sca_plan.rule_count_for_product(out.Product(out.SCA()))

    secrets_rule_count = sast_plan.rule_count_for_product(out.Product(out.Secrets()))
    has_secret_rules = secrets_rule_count > 0

    # NOTE: Currently, on the `scan` command without a --config argument provided
    # is eligible for the simple UX.
    if simple_ux:
        # Print the feature summary table instead of all tables with new simple CLI UX
        _print_product_status(
            sast_enabled=with_code_rules,
            sca_enabled=with_supply_chain,
        )
        return plans

    if not has_sca_rules and not has_secret_rules and legacy_ux:
        # Print these SAST table without section headers
        _print_sast_table(
            sast_plan=sast_plan,
            product=out.Product(out.SAST()),
            rule_count=alt_sast_rule_count,
        )
        return plans

    if legacy_ux:
        console.print(Padding(Title("Code Rules", order=2), (1, 0, 0, 0)))
    else:
        console.print(Title("Code Rules", order=2))

    _print_sast_table(
        sast_plan=sast_plan,
        product=out.Product(out.SAST()),
        rule_count=alt_sast_rule_count,
    )

    # TODO: after launch this should no longer be conditional.
    if has_secret_rules:
        console.print(Title("Secrets Rules", order=2))
        _print_sast_table(
            sast_plan=sast_plan,
            product=out.Product(out.Secrets()),
            rule_count=secrets_rule_count,
        )

    if not has_sca_rules and legacy_ux:
        pass  # Skip showing an empty supply chain rules section for legacy ux
    elif legacy_ux:
        # Show the basic table for supply chain
        console.print(Title("Supply Chain Rules", order=2))
        _print_sca_table(sca_plan=sca_plan, rule_count=alt_sca_rule_count)
    else:
        # Show the table with a supply chain nudge or supply chain
        console.print(Title("Supply Chain Rules", order=2))
        _print_detailed_sca_table(
            sca_plan=sca_plan,
            rule_count=alt_sca_rule_count,
            # NOTE: `with_supply_chain` is only used for nudging `scan` command invocations
            # without supply-chain to upgrade their usage to the `ci` command
            with_supply_chain=with_supply_chain,
        )

    if detailed_ux:
        console.print(Title("Progress", order=2))
        console.print(" ")  # space intentional for progress bar padding

    return plans
