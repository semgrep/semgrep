from typing import Any, Dict, List, Sequence, Set, Tuple, Optional, Collection
from typing import Union
from pathlib import Path
import semgrep.output as output

def invoke_semgrep(
    config: Path,
    targets: List[Path],
    output_settings: Optional[output.OutputSettings] = None,
    **kwargs: Any,
) -> Union[Dict[str, Any], str]:
    """
    Return Semgrep results of 'config' on 'targets' as a dict|str
    Uses default arguments of 'semgrep_main.main' unless overwritten with 'kwargs'
    """
    ...

# TODO: also main(), special case that if called from somewhere??
