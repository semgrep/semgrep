from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
import logging

class VerboseLogging(logging.Logger):
    """
    Extend logging to add a verbose logging level that is between
    INFO and DEBUG.

    Also expose a logging.verbose() method so there is no need
    to call log(VERBOSE_LEVEL, msg) every time
    """
    ...

def getLogger(name: Optional[str]) -> VerboseLogging:
    """
    Wrapper around logging.getLogger to correctly cast so mypy
    detects verbose() function
    """
    ...
