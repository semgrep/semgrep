import os
import subprocess
from pathlib import Path
from typing import Any
from typing import Optional
from typing import Set

import requests
import urllib3
from attrs import define
from attrs import field

from semgrep import __VERSION__


@define
class UserAgent:
    """
    Generates the user agent string we send to Semgrep App.

    >>> from semgrep.state import get_state
    >>> app_session = get_state().app_session
    >>> str(app_session.user_agent)
    'semgrep/0.1.2'
    >>> app_session.user_agent.tags.add("testing")
    >>> str(app_session.user_agent)
    'semgrep/0.1.2 (testing)'
    """

    name: str = field(default=f"Semgrep", init=False)
    version: str = field(default=__VERSION__, init=False)
    tags: Set[str] = field(init=False)

    @tags.default
    def get_default_tags(self) -> Set[str]:
        result = set()
        if os.getenv("SEMGREP_USER_AGENT_APPEND"):
            result.add(os.environ["SEMGREP_USER_AGENT_APPEND"])

        try:
            # nosem: use-git-check-output-helper
            remote_url = subprocess.check_output(
                ["git", "remote", "get-url", "origin"],
                cwd=Path(__file__).parent,
                stderr=subprocess.DEVNULL,
                encoding="utf-8",
            ).strip()
            # nosem: use-git-check-output-helper
            sha = subprocess.check_output(
                [
                    "git",
                    "describe",
                    # a --match value never matches will give us SHA instead of most recent tag
                    "--match=nah_dont_actually_match",
                    "--always",
                    "--dirty",
                ],
                cwd=Path(__file__).parent,
                stderr=subprocess.DEVNULL,
                encoding="utf-8",
            ).strip()
            # If we installed semgrep in `.venv/` in the semgrep-docs repo,
            # this function would report semgrep-docs commit SHAs in the user agent.
            # This is why we verify the origin URL, and why check with .endswith()
            if remote_url.replace(".git", "").endswith("/semgrep"):
                result.add(f"sha/{sha}")
        except (OSError, subprocess.CalledProcessError):
            pass

        return result

    def __str__(self) -> str:
        result = f"{self.name}/{self.version}"
        for note in sorted(self.tags, key=lambda x: ("/" in x, x)):
            clean_note = note.strip("()")  # sometimes the env var has parens already
            result += f" ({clean_note})"
        return result


def enhance_ssl_error_message(
    err: requests.exceptions.SSLError,
) -> requests.exceptions.SSLError:
    """
    If the provided SSLError wraps a SSL hostname mismatch exception, re-create the SSLError with a more descriptive error message.
    """
    inner_err: Optional[Exception] = None

    if err.args:
        inner_err = err.args[0]

    if isinstance(inner_err, urllib3.connectionpool.MaxRetryError) and inner_err.reason:
        inner_err = inner_err.reason

    if isinstance(inner_err, requests.exceptions.SSLError) and inner_err.args:
        inner_err = inner_err.args[0]

    if (
        isinstance(inner_err, urllib3.connectionpool.CertificateError)
        and inner_err.args
        and isinstance(inner_err.args[0], str)
        and inner_err.args[0].startswith("hostname")
        and "doesn't match" in inner_err.args[0]
    ):
        return requests.exceptions.SSLError(
            f"SSL certificate error: {inner_err.args[0]}. This error typically occurs when your internet traffic is being routed through a proxy. If this is the case, try setting the REQUESTS_CA_BUNDLE environment variable to the location of your proxy's CA certificate."
        )

    return err


class AppSession(requests.Session):
    """
    Send requests to Semgrep App with this session.

    Use the instance at `semgrep.app.app_session` instead of creating a new one.

    The following features are added over the base Session class:
    - A default retrying policy is added to each request
    - A User-Agent is automatically added to each request
    - A default timeout of 30 seconds is added to each request
    - If a token is available, it is added to the request as an Authorization header

    Normal usage:
    >>> from semgrep.state import get_state
    >>> app_session = get_state().app_session
    >>> app_session.get(url)

    Disable custom user agent for a request:
    >>> app_session.get(url, headers={"User-Agent": None}))

    Disable timeout for a request:
    >>> app_session.get(url, timeout=None)

    Disable authentication for a request:
    >>> app_session.get(url, headers={"Authorization": None})
    """

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)
        self.user_agent = UserAgent()
        self.token: Optional[str] = None

        # retry after 4, 8, 16 seconds
        retry_adapter = requests.adapters.HTTPAdapter(
            max_retries=urllib3.Retry(
                total=3,
                backoff_factor=4,
                other=0,
                allowed_methods=["GET", "POST"],
                status_forcelist=(413, 429, 500, 502, 503),
            ),
        )

        self.mount("https://", retry_adapter)
        self.mount("http://", retry_adapter)

    def authenticate(self) -> None:
        # avoid circular imports in semgrep.state
        from semgrep.app import auth
        from semgrep.state import get_state

        self.token = auth.get_token()

        metrics = get_state().metrics
        metrics.add_token(self.token)

    def request(self, *args: Any, **kwargs: Any) -> requests.Response:
        kwargs.setdefault("timeout", 60)
        kwargs.setdefault("headers", {})
        kwargs["headers"].setdefault("User-Agent", str(self.user_agent))
        if self.token:
            kwargs["headers"].setdefault("Authorization", f"Bearer {self.token}")

        from semgrep.state import get_state

        error_handler = get_state().error_handler
        method, url = args
        error_handler.push_request(method, url, **kwargs)
        try:
            response = super().request(*args, **kwargs)
        except requests.exceptions.SSLError as err:
            raise enhance_ssl_error_message(err)

        if response.ok:
            error_handler.pop_request()
        else:
            error_handler.append_request(status_code=response.status_code)
        return response
