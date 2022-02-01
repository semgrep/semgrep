from semgrep.util import with_color
from semgrep.verbose_logging import getLogger

HAS_SHOWN_SETTINGS_KEY = "has_shown_metrics_notification"

logger = getLogger(__name__)


def possibly_notify_user() -> None:
    from semgrep.settings import SETTINGS

    has_shown = False
    try:
        has_shown = SETTINGS.get_setting(HAS_SHOWN_SETTINGS_KEY, default=False)
    except PermissionError:
        logger.debug("Semgrep does not have access to user settings file")

    if not has_shown:
        logger.warning(
            with_color(
                "yellow",
                "METRICS: Using configs from the Registry (like --config=p/ci) reports pseudonymous rule metrics to semgrep.dev."
                """\nTo disable Registry rule metrics, use "--metrics=off"."""
                "\nUsing configs only from local files (like --config=xyz.yml) does not enable metrics."
                "\n"
                "\nMore information: https://semgrep.dev/docs/metrics"
                "\n",
            )
        )
        SETTINGS.add_setting(HAS_SHOWN_SETTINGS_KEY, True)
