import os
from datetime import timedelta
from typing import Any
from typing import Optional
from typing import Set

import requests
import requests_cache
import urllib3
from attrs import define
from attrs import field

from semgrep import __VERSION__
from semgrep.constants import USER_DATA_FOLDER


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
        if os.getenv("SEMGREP_USER_AGENT_APPEND"):
            return set({os.environ["SEMGREP_USER_AGENT_APPEND"]})
        return set()

    def __str__(self) -> str:
        result = f"{self.name}/{self.version}"
        for note in sorted(self.tags):
            clean_note = note.strip("()")  # sometimes the env var has parens already
            result += f" ({clean_note})"
        return result


REQUESTS_CACHE_PATH = USER_DATA_FOLDER / "requests-cache.sqlite"


class AppSession(requests_cache.CacheMixin, requests.Session):  # type: ignore
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
        super().__init__(  # type: ignore
            str(REQUESTS_CACHE_PATH), *args, expire_after=timedelta(days=30), **kwargs
        )
        self.user_agent = UserAgent()
        self.token: Optional[str] = None

        # retry after 4, 8, 16 seconds
        retry_adapter = requests.adapters.HTTPAdapter(
            max_retries=urllib3.Retry(
                total=3,
                backoff_factor=4,
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
        metrics.set_is_authenticated(bool(self.token))

    def request(self, *args: Any, **kwargs: Any) -> requests.Response:
        kwargs.setdefault("timeout", 30)
        kwargs.setdefault("headers", {})
        kwargs.setdefault("refresh", True)
        kwargs["headers"].setdefault("User-Agent", str(self.user_agent))
        if self.token:
            kwargs["headers"].setdefault("Authorization", f"Bearer {self.token}")
        return super().request(*args, **kwargs)
