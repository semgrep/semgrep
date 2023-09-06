import os
from pathlib import Path
from typing import Iterable
from typing import Optional
from typing import overload
from typing import Union

from attr import Factory
from attr import field
from attr import frozen

from semgrep.constants import SETTINGS_FILENAME


def url(value: str) -> str:
    return value.rstrip("/")


def migrate_fail_open_url(value: str) -> str:
    # Supports the fail_open_url being the hostname without the path even when some folks might set the path
    return url(value.replace("/failure", ""))


@overload
def EnvFactory(envvars: Union[str, Iterable[str]], default: str) -> str:
    ...


@overload
def EnvFactory(envvars: Union[str, Iterable[str]]) -> Optional[str]:
    ...


def EnvFactory(
    envvars: Union[str, Iterable[str]], default: Optional[str] = None
) -> Optional[str]:
    if isinstance(envvars, str):
        envvars = [envvars]

    def env_getter() -> Optional[str]:
        for envvar in envvars:
            if os.getenv(envvar):
                return os.getenv(envvar)
        return default

    return Factory(env_getter)


@frozen
class Env:
    """Returns the value of an environment variable at the time of command invocation.

    This is better than just keeping these values as constants on the module level,
    because tests and other non-CLI based invocations might change env variables
    between multiple invocations.
    """

    fail_open_url: str = field(
        default=EnvFactory(
            ["SEMGREP_FAIL_OPEN_URL"],
            "https://fail-open.prod.semgrep.dev",
        ),
        converter=migrate_fail_open_url,
    )
    semgrep_url: str = field(
        default=EnvFactory(["SEMGREP_URL", "SEMGREP_APP_URL"], "https://semgrep.dev"),
        converter=url,
    )
    app_token: Optional[str] = field(default=EnvFactory("SEMGREP_APP_TOKEN"))

    version_check_url: str = field(
        default=EnvFactory(
            "SEMGREP_VERSION_CHECK_URL", "https://semgrep.dev/api/check-version"
        )
    )
    version_check_timeout: int = field()
    version_check_cache_path: Path = field()

    git_command_timeout: int = field()

    src_directory: Path = field()
    user_data_folder: Path = field()
    user_log_file: Path = field()
    user_settings_file: Path = field()

    in_docker: bool = field()
    in_gh_action: bool = field()
    in_agent: bool = field()
    with_new_cli_ux: bool = field()
    min_fetch_depth: int = field()

    upload_findings_timeout: int = field()

    r2c_internal_jsonnet_lib: Path = field()

    @version_check_timeout.default
    def version_check_timeout_default(self) -> int:
        value = os.getenv("SEMGREP_VERSION_CHECK_TIMEOUT", "2")
        return int(value)

    @version_check_cache_path.default
    def version_check_cache_path_default(self) -> Path:
        value = os.getenv("SEMGREP_VERSION_CACHE_PATH")
        if value:
            return Path(value)
        return Path.home() / ".cache" / "semgrep_version"

    @git_command_timeout.default
    def git_command_timeout_default(self) -> int:
        value = os.getenv("SEMGREP_GIT_COMMAND_TIMEOUT", "300")
        return int(value)

    @src_directory.default
    def src_directory_default(self) -> Path:
        value = os.getenv("SEMGREP_SRC_DIRECTORY")
        if value:
            return Path(value)
        return Path("/src")

    @user_data_folder.default
    def user_data_folder_default(self) -> Path:
        config_home = os.getenv("XDG_CONFIG_HOME")
        if config_home is None or not Path(config_home).is_dir():
            parent_dir = Path.home()
        else:
            parent_dir = Path(config_home)
        return parent_dir / ".semgrep"

    @user_log_file.default
    def user_log_file_default(self) -> Path:
        path = os.getenv("SEMGREP_LOG_FILE", str(self.user_data_folder / "semgrep.log"))
        return Path(path)

    @user_settings_file.default
    def user_settings_file_default(self) -> Path:
        path = os.getenv(
            "SEMGREP_SETTINGS_FILE", str(self.user_data_folder / SETTINGS_FILENAME)
        )
        return Path(path)

    @in_docker.default
    def in_docker_default(self) -> bool:
        return "SEMGREP_IN_DOCKER" in os.environ

    @in_gh_action.default
    def in_gh_action_default(self) -> bool:
        return "GITHUB_WORKSPACE" in os.environ

    @in_agent.default
    def in_agent_default(self) -> bool:
        return "SEMGREP_AGENT" in os.environ

    @with_new_cli_ux.default
    def with_new_cli_default(self) -> bool:
        return os.environ.get("SEMGREP_NEW_CLI_UX", "0") == "1"

    @min_fetch_depth.default
    def min_fetch_depth_default(self) -> int:
        value = os.getenv("SEMGREP_GHA_MIN_FETCH_DEPTH", "0")
        return int(value)

    # R2C_INTERNAL_JSONNET
    @r2c_internal_jsonnet_lib.default
    def r2c_internal_jsonnet_lib_default(self) -> Path:
        value = os.getenv("R2C_INTERNAL_JSONNET_LIB")
        if value:
            return Path(value)
        # TODO what should the default path be?
        return Path.home()

    @upload_findings_timeout.default
    def upload_findings_timeout_default(self) -> int:
        value = os.getenv("SEMGREP_UPLOAD_FINDINGS_TIMEOUT", "300")
        return int(value)
