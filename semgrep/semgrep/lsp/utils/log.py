import logging
import sys
from typing import Optional


def init_log(name: str, level: int, logfile: Optional[str] = None) -> None:
    log = logging.getLogger(name)
    log.setLevel(level)
    handler: logging.Handler = logging.StreamHandler(stream=sys.stderr)
    if logfile:
        handler = logging.FileHandler(logfile)
    handler.setFormatter(
        logging.Formatter("%(asctime)s - %(name)s - %(levelname)s - %(message)s")
    )
    log.addHandler(handler)
