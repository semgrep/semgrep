from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

HAS_SHOWN_SETTINGS_KEY = "has_shown_metrics_notification"

logger = getLogger(__name__)


def possibly_notify_user() -> None:
    from semgrep.settings import SETTINGS

    has_shown = False
    try:
        has_shown = SETTINGS.value.get(HAS_SHOWN_SETTINGS_KEY, False)
    except PermissionError:
        logger.debug("Semgrep does not have access to user settings file")

    if not has_shown:
        logger.warning(
            with_color(
                "yellow",
                "NOTICE: Semgrep may send pseudonymous usage metrics to its server."
                "\nBy default, Semgrep will only send usage metrics when rules are pulled from Semgrep's servers."
                "\nTo disable metrics collection, run Semgrep with --metrics off."
                "\n",
            )
        )
        SETTINGS.value = {**SETTINGS.value, HAS_SHOWN_SETTINGS_KEY: True}
