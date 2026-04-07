#
# Copyright (c) 2025 Semgrep Inc.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public License
# version 2.1 as published by the Free Software Foundation.
#
# This library is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
# LICENSE for more details.
#
import asyncio
import json
import os
import re
import sys
from typing import Literal

from opentelemetry import trace
from pydantic import BaseModel

from semgrep.mcp.hooks.settings import load_hook_settings
from semgrep.mcp.models import SupplyChainFinding
from semgrep.mcp.semgrep import run_semgrep_process_sync
from semgrep.mcp.server import get_semgrep_app_token
from semgrep.mcp.utilities.tracing import attach_agent_info
from semgrep.mcp.utilities.tracing import attach_git_info
from semgrep.mcp.utilities.tracing import start_tracing
from semgrep.mcp.utilities.tracing import with_hook_span
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)


# ---------------------------------------------------------------------------------
# Models
# ---------------------------------------------------------------------------------


class PostToolHookResponse(BaseModel):
    decision: Literal["block"] | None = None
    reason: str | None = None


# ---------------------------------------------------------------------------------
# Utils for loading hook data
# ---------------------------------------------------------------------------------


def load_bash_command() -> tuple[str, str]:
    """
    Returns (command, cwd) from a PostToolUse Bash hook payload.
    """
    hook_data = json.load(sys.stdin)
    print(hook_data, file=sys.stderr)
    command = hook_data["tool_input"]["command"]
    cwd = hook_data.get("cwd", os.getcwd())
    return command, cwd


# ---------------------------------------------------------------------------------
# Hook
# ---------------------------------------------------------------------------------


@with_hook_span(
    span_name="semgrep_scan_supply_chain (hook)",
    send_metrics=True,
    is_semgrep_scan=True,
)
async def run_supply_chain_scan(
    top_level_span: trace.Span | None, cwd: str
) -> PostToolHookResponse:
    attach_git_info(trace.get_current_span(), cwd)

    original_dir = os.getcwd()
    try:
        os.chdir(cwd)
        args = ["scan", "--config", "supply-chain", "--json"]
        process = await run_semgrep_process_sync(top_level_span, args)
    finally:
        os.chdir(original_dir)

    if not process.stdout:
        return PostToolHookResponse()

    resp_json = json.loads(process.stdout.decode())
    results = resp_json.get("results", [])
    logger.info(f"Supply chain scan results: {results}")
    if results:
        findings = [SupplyChainFinding.model_validate(r) for r in results]
        reachable = [
            f
            for f in findings
            if (
                # If the sca_info is None, we assume it's reachable
                # so we don't miss any findings.
                (f.extra.sca_info and f.extra.sca_info.reachable)
                or f.extra.sca_info is None
            )
        ]
        if reachable:
            return PostToolHookResponse(
                decision="block",
                reason=str(
                    [
                        {
                            "check_id": f.check_id,
                            "path": f.path,
                            "start_line": f.start.line,
                            "start_column": f.start.col,
                            "start_offset": f.start.offset,
                            "message": f.extra.message,
                            "severity": f.extra.severity,
                        }
                        for f in reachable
                    ]
                ),
            )
        else:
            logger.info("No reachable findings, exiting...")
            return PostToolHookResponse()
    else:
        return PostToolHookResponse()


# ---------------------------------------------------------------------------------
# Entry point
# ---------------------------------------------------------------------------------


def run_supply_chain_scan_cli(agent: str) -> None:
    with start_tracing("mcp-hook") as span:
        attach_agent_info(span, agent)

        command, cwd = load_bash_command()

        settings = load_hook_settings(cwd)
        if settings.disable_supply_chain_scan:
            logger.info("Supply chain scan is disabled, exiting...")
            sys.exit(0)

        # This is not the best way to check if a command is an install command,
        # since this is not an exhaustive list, and we are just checking if these
        # things are substrings of the command. But it's good enough for now.
        _INSTALL_COMMANDS = [
            "pip install",
            "npm install",
            "npm i",
            "npm ci",
            "gem install",
            "uv sync",  # uv
            "uv add",
            "pip-sync",  # pip-tools
            "yarn install",
            "yarn add",
            "pnpm install",  # pnpm
            "bundle install",  # bundler (Ruby)
            "composer install",  # composer (PHP)
            "cargo add",  # cargo (Rust)
            "cargo install",
            "go get",  # go modules
            "go mod tidy",
            "mvn install",  # Maven
            "gradle dependencies",  # Gradle
            "poetry install",  # poetry
            "pipenv install",  # pipenv
        ]
        if not any(
            re.search(re.escape(cmd) + r"(\s|$)", command.lower())
            for cmd in _INSTALL_COMMANDS
        ):
            sys.exit(0)

        # TODO: this doesn't actually check if the token is valid
        if get_semgrep_app_token() is None:
            print(
                "No SEMGREP_APP_TOKEN found, please login to Semgrep to use this hook.",
                file=sys.stderr,
            )
            sys.exit(2)

        response = asyncio.run(run_supply_chain_scan(span, cwd))
        print(response.model_dump_json(exclude_none=True))
        sys.exit(0)
