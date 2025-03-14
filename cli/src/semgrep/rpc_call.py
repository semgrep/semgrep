from typing import List
from typing import Optional
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep.rpc import rpc_call
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

##############################################################################
# The calls to OCaml
##############################################################################


def format(
    formatter: out.OutputFormat, ctx: out.FormatContext, output: out.CliOutput
) -> str:
    call = out.FunctionCall(out.CallFormatter((formatter, ctx, output)))
    ret: Optional[out.RetFormatter] = rpc_call(call, out.RetFormatter)
    if ret is None:
        return "<ERROR: missing output>"
    return ret.value


def apply_fixes(args: out.ApplyFixesParams) -> Optional[out.ApplyFixesReturn]:
    call = out.FunctionCall(out.CallApplyFixes(args))
    ret: Optional[out.RetApplyFixes] = rpc_call(call, out.RetApplyFixes)
    if ret is None:
        # No real point in logging here. We log for each of the conditions that
        # could cause this, and we log in the caller too.
        return None
    return ret.value


def sarif_format(
    sarif_format: out.SarifFormat, ctx: out.FormatContext, cli_out: out.CliOutput
) -> Optional[out.RetSarifFormat]:
    call = out.FunctionCall(out.CallSarifFormat((sarif_format, ctx, cli_out)))
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


def validate(fp: out.Fpath) -> bool:
    call = out.FunctionCall(out.CallValidate(fp))
    ret: Optional[out.RetValidate] = rpc_call(call, out.RetValidate)
    if ret is None:
        logger.error("Failed to validate semgrep configuration")
        return out.RetValidate(False).value
    return ret.value


def resolve_dependencies(
    dependency_sources: List[out.DependencySource],
    download_dependency_source_code: bool,
) -> Optional[List[Tuple[out.DependencySource, out.ResolutionResult]]]:
    call = out.FunctionCall(
        out.CallResolveDependencies(
            out.ResolveDependenciesParams(
                dependency_sources, download_dependency_source_code
            )
        )
    )
    ret: Optional[out.RetResolveDependencies] = rpc_call(
        call, out.RetResolveDependencies
    )
    if ret is None:
        logger.warning("failed to resolve dependencies")
        return None
    return ret.value


def upload_symbol_analysis(
    token: str, scan_id: int, symbol_analysis: out.SymbolAnalysis
) -> None:
    call = out.FunctionCall(
        out.CallUploadSymbolAnalysis((token, scan_id, symbol_analysis))
    )
    ret: Optional[out.RetUploadSymbolAnalysis] = rpc_call(
        call, out.RetUploadSymbolAnalysis
    )
    if ret is None:
        logger.warning(
            "Failed to upload symbol analysis, somehow. Continuing with scan..."
        )
    else:
        logger.debug(f"Uploading symbol analysis succeeded with {ret.value}")


def transitive_reachability_filter(
    args: out.TransitiveReachabilityFilterParams,
) -> List[out.TransitiveFinding]:
    call = out.FunctionCall(out.CallTransitiveReachabilityFilter(args))
    ret: Optional[out.RetTransitiveReachabilityFilter] = rpc_call(
        call, out.RetTransitiveReachabilityFilter
    )
    if ret is None:
        logger.warning("failed to filter transitive findings")
        # return the same findings
        return args.findings
    return ret.value


def dump_rule_partitions(args: out.DumpRulePartitionsParams) -> bool:
    call = out.FunctionCall(out.CallDumpRulePartitions(args))
    ret: Optional[out.RetDumpRulePartitions] = rpc_call(call, out.RetDumpRulePartitions)
    if ret is None:
        logger.error("Failed to dump rule partitions")
        return out.RetDumpRulePartitions(False).value
    return ret.value


def get_targets(scanning_roots: out.ScanningRoots) -> out.TargetDiscoveryResult:
    call = out.FunctionCall(out.CallGetTargets(scanning_roots))
    ret: Optional[out.RetGetTargets] = rpc_call(call, out.RetGetTargets)
    if ret is None:
        logger.error("Failed to obtain target files from semgrep-core")
        return out.TargetDiscoveryResult([], [], [])
    logger.debug(f"get_targets request: {scanning_roots}\n..... result: {ret.value}")
    return ret.value

def match_subprojects(dependency_source_files: List[out.Fpath]) -> List[out.Subproject]:
    call = out.FunctionCall(out.CallMatchSubprojects(dependency_source_files))
    ret: Optional[out.RetMatchSubprojects] = rpc_call(call, out.RetMatchSubprojects)
    if ret is None:
        logger.error("Failed to match subprojects")
        return []
    return ret.value
