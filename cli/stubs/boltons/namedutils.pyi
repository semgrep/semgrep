from __future__ import annotations

from collections import OrderedDict
from operator import itemgetter

def exec_(
    code: str,
    global_env: dict[
        str,
        type[itemgetter] | str | type[OrderedDict] | type[property] | type[tuple],
    ],
) -> None: ...
