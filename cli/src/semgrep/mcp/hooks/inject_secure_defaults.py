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
import sys
import tempfile
import time
from pathlib import Path

import httpx
from opentelemetry import trace
from pydantic import BaseModel

from semgrep.mcp.utilities.tracing import start_tracing
from semgrep.mcp.utilities.tracing import with_hook_span

CACHE_FILE = (
    Path(tempfile.gettempdir()) / "semgrep-mcp" / "claude-secure-defaults-cache.md"
)
CACHE_FILE.parent.mkdir(parents=True, exist_ok=True)
CACHE_MAX_AGE = 86400  # 24 hours in seconds
README_URL = (
    "https://raw.githubusercontent.com/tldrsec/awesome-secure-defaults/main/README.md"
)


class HookSpecificOutput(BaseModel):
    hookEventName: str
    additionalContext: str


class UserPromptSubmitHookResponse(BaseModel):
    hookSpecificOutput: HookSpecificOutput


def get_hook_event_name() -> str:
    hook_data = json.load(sys.stdin)
    return str(hook_data["hook_event_name"])


def is_cache_fresh() -> bool:
    """Check if cache exists and is fresh (less than 24 hours old)."""
    if not CACHE_FILE.exists():
        return False

    cache_age = time.time() - CACHE_FILE.stat().st_mtime
    return cache_age <= CACHE_MAX_AGE


async def fetch_readme() -> str:
    """Fetch the README from GitHub and cache it."""
    async with httpx.AsyncClient(timeout=10.0) as client:
        try:
            response = await client.get(README_URL)
            response.raise_for_status()
            content = response.text

            # Cache the content
            CACHE_FILE.touch(exist_ok=True)
            CACHE_FILE.write_text(content)
            return content
        except Exception as e:
            print(f"Error fetching README: {e}", file=sys.stderr)
            # Return fallback content if fetch fails
            return get_fallback_content()


def get_cached_content() -> str | None:
    """Get content from cache if available."""
    if CACHE_FILE.exists():
        return CACHE_FILE.read_text()
    return None


def get_fallback_content() -> str:
    """Return fallback content if GitHub fetch fails."""
    return """## Security Guidance: Secure-by-Default Libraries

    When writing code, consider using these security-focused libraries that follow secure-by-default principles:

    ### Key Security Libraries by Category:

    **HTTP Security Headers**
    - Helmet.js (Node.js), secure_headers (Ruby) - Set protective HTTP response headers

    **XSS Prevention**
    - DOMPurify (JS), Bleach (Python), Ammonia (Rust) - Sanitize HTML safely

    **CSRF Protection**
    - Gorilla CSRF (Go), anti-csrf (PHP) - Token-based CSRF defense

    **Cryptography**
    - Google Tink (multi-language) - Secure, easy-to-use crypto APIs
    - Themis (14+ platforms) - High-level crypto framework

    **Input Validation**
    - Safe-regex - Detect catastrophic regex
    - defusedxml (Python) - Prevent XML attacks

    **SSRF Defense**
    - ssrf_filter (Ruby), ssrf-req-filter (Node.js) - Block private IP connections

    **Deserialization**
    - SerialKiller (Java) - Safe deserialization

    **Template Engines**
    - Mustache, Handlebars, Liquid - Logic-less templates prevent injection

    For detailed information, see: https://github.com/tldrsec/awesome-secure-defaults

    💡 When implementing security features, prefer these well-tested libraries over custom solutions."""


async def get_secure_defaults_context(inject_short_context: bool) -> str:
    """Get secure defaults context, using cache if fresh or fetching if needed."""
    if inject_short_context:
        return get_fallback_content()

    if is_cache_fresh():
        content = get_cached_content()
        if content is not None:
            return content

    # Cache is stale or doesn't exist, fetch new content
    return await fetch_readme()


@with_hook_span(
    span_name="inject_secure_defaults_context (hook)",
    send_metrics=True,
    is_semgrep_scan=False,
)
async def run_inject_secure_defaults_hook_async(
    top_level_span: trace.Span | None, inject_short_context: bool
) -> UserPromptSubmitHookResponse:
    """Main hook logic to inject security guidance."""
    content = await get_secure_defaults_context(inject_short_context)
    additional_context = f"""## Security Guidance: Secure-by-Default Libraries

    When writing code, consider using these security-focused libraries that follow secure-by-default principles:

    {content}

    💡 When implementing security features, prefer these well-tested libraries over custom solutions.
    For detailed information, see: https://github.com/tldrsec/awesome-secure-defaults"""

    return UserPromptSubmitHookResponse(
        hookSpecificOutput=HookSpecificOutput(
            hookEventName=get_hook_event_name(),
            additionalContext=additional_context,
        )
    )


def run_inject_secure_defaults_hook(
    agent: str, inject_short_context: bool = False
) -> None:
    """
    Entry point for the inject secure defaults hook.
    If `inject_short_context` is True, the fallback content will be injected instead of the context
    from the README on the GitHub repo.
    """
    with start_tracing("mcp-hook") as span:
        if agent == "cursor":
            # This hook is not supported for Cursor yet because
            # Cursor's beforeSubmitPrompt does not support
            # injecting context. See: https://cursor.com/docs/agent/hooks#beforesubmitprompt
            #
            # There is also no way to inject context at the start of a Cursor session at the moment.
            print("This hook is not supported for Cursor.", file=sys.stderr)
            sys.exit(2)

        response = asyncio.run(
            run_inject_secure_defaults_hook_async(
                span, inject_short_context=inject_short_context
            )
        )
        print(response.model_dump_json(exclude_none=True))
        sys.exit(0)
