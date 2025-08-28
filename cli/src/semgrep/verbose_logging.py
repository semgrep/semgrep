#
# Copyright (c) 2021-2024 Semgrep Inc.
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
import logging
from typing import Any
from typing import cast
from typing import Optional


class VerboseLogging(logging.Logger):
    """
    Extend logging to add a verbose logging level that is between
    INFO and DEBUG.

    Also expose a logging.verbose() method so there is no need
    to call log(VERBOSE_LEVEL, msg) every time
    """

    VERBOSE_LOG_LEVEL = 15

    def __init__(self, *args: Any, **kwargs: Any) -> None:
        super().__init__(*args, **kwargs)

    def verbose(self, msg: str, *args: Any, **kwargs: Any) -> None:
        if self.isEnabledFor(self.VERBOSE_LOG_LEVEL):
            self._log(self.VERBOSE_LOG_LEVEL, msg, args, **kwargs)


def install_verbose_logging() -> None:
    """
    Makes 3 changes to stdlib logging:
    - add in logging.VERBOSE constant
    - add VERBOSE as a logging level
    - set VerboseLogging as default class returned by logging.getLogger
        - thus exposing logger.verbose(msg) method

    Any calls to getLogger before this method returns will return base
    logging.Logger class that doesn't have verbose() convenience method
    """
    # Between INFO(20) and DEBUG(10)
    logging.VERBOSE = 15  # type: ignore[attr-defined]
    logging.addLevelName(
        VerboseLogging.VERBOSE_LOG_LEVEL, "VERBOSE"
    )  # Register VERBOSE as a logging level
    logging.setLoggerClass(VerboseLogging)


install_verbose_logging()


def getLogger(name: Optional[str]) -> VerboseLogging:
    """
    Wrapper around logging.getLogger to correctly cast so mypy
    detects verbose() function
    """
    return cast(VerboseLogging, logging.getLogger(name))
