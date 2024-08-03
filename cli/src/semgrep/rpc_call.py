from typing import Optional

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.rpc import rpc_call
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

##############################################################################
# The calls to OCaml
##############################################################################


def format(formatter: out.OutputFormat, output: out.CliOutput) -> str:
    call = out.FunctionCall(out.CallFormatter((formatter, output)))
    ret: Optional[out.RetFormatter] = rpc_call(call, out.RetFormatter)
    if ret is None:
        return "<error missing output>"
    return ret.value


def apply_fixes(args: out.ApplyFixesParams) -> Optional[out.ApplyFixesReturn]:
    call = out.FunctionCall(out.CallApplyFixes(args))
    ret: Optional[out.RetApplyFixes] = rpc_call(call, out.RetApplyFixes)
    if ret is None:
        # No real point in logging here. We log for each of the conditions that
        # could cause this, and we log in the caller too.
        return None
    return ret.value


def sarif_format(args: out.SarifFormatParams) -> Optional[out.RetSarifFormat]:
    call = out.FunctionCall(out.CallSarifFormat(args))
    ret: Optional[out.RetSarifFormat] = rpc_call(call, out.RetSarifFormat)
    if ret is None:
        # No real point in logging here. We log for each of the conditions that
        # could cause this, and we log in the caller too.
        return None
    return ret


def contributions() -> out.Contributions:
    call = out.FunctionCall(out.CallContributions())
    ret: Optional[out.RetContributions] = rpc_call(call, out.RetContributions)
    if ret is None:
        logger.warning("Failed to collect contributions. Continuing with scan...")
        return out.Contributions([])
    return ret.value
