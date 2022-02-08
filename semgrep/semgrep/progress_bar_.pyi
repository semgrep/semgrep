from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from typing import Iterable, IO
import sys
from semgrep.util import T

def debug_tqdm_write(msg: str, file: IO = sys.stderr) -> None: ...
def progress_bar(
    iterable: Iterable[T], file: IO = sys.stderr, **kwargs: Any
) -> Iterable[T]:
    """
    Return tqdm-wrapped iterable if output stream is a tty;
    else return iterable without tqdm.
    """
    ...
