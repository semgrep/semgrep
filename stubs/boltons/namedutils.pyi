from collections import OrderedDict
from operator import itemgetter
from typing import Dict, Type, Union

def exec_(
    code: str,
    global_env: Dict[
        str,
        Union[
            Type[itemgetter],
            str,
            Type[OrderedDict],
            Type[property],
            Type[tuple],
        ],
    ],
) -> None: ...
