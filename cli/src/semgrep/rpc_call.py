#
# Copyright (c) 2024-2025 Semgrep Inc.
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
from typing import List
from typing import Optional
from typing import Tuple

import semgrep.semgrep_interfaces.semgrep_output_v1 as out
from semgrep import tracing
from semgrep.rpc import rpc_call
from semgrep.verbose_logging import getLogger

logger = getLogger(__name__)

##############################################################################
# The calls to OCaml
##############################################################################


@tracing.trace()
def format(
    formatter: out.OutputFormat, ctx: out.FormatContext, output: out.CliOutput
) -> str:
    call = out.FunctionCall(out.CallFormatter((formatter, ctx, output)))
    ret: Optional[out.RetFormatter] = rpc_call(call, out.RetFormatter)
    if ret is None:
        return "<ERROR: missing output>"
    return ret.value


@tracing.trace()
def apply_fixes(args: out.ApplyFixesParams) -> Optional[out.ApplyFixesReturn]:
    call = out.FunctionCall(out.CallApplyFixes(args))
    ret: Optional[out.RetApplyFixes] = rpc_call(call, out.RetApplyFixes)
    if ret is None:
        # No real point in logging here. We log for each of the conditions that
        # could cause this, and we log in the caller too.
        return None
    return ret.value


@tracing.trace()
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


@tracing.trace()
def contributions() -> out.Contributions:
    call = out.FunctionCall(out.CallContributions())
    ret: Optional[out.RetContributions] = rpc_call(call, out.RetContributions)
    if ret is None:
        logger.warning("Failed to collect contributions. Continuing with scan...")
        return out.Contributions([])
    return ret.value


@tracing.trace()
def validate(fp: out.Fpath) -> Optional[out.CoreError]:
    call = out.FunctionCall(out.CallValidate(fp))
    ret: Optional[out.RetValidate] = rpc_call(call, out.RetValidate)
    if ret is None:
        logger.error("Failed to validate semgrep configuration")
        return out.CoreError(
            error_type=out.ErrorType(out.SemgrepError()),
            severity=out.ErrorSeverity(out.Error_()),
            message=f"Failed to validate rule configuration at {fp.value}",
        )
    return ret.value


@tracing.trace()
def resolve_dependencies(
    dependency_sources: List[out.DependencySource],
    download_dependency_source_code: bool,
    allow_local_builds: bool,
) -> Optional[List[Tuple[out.DependencySource, out.ResolutionResult]]]:
    call = out.FunctionCall(
        out.CallResolveDependencies(
            out.ResolveDependenciesParams(
                dependency_sources, download_dependency_source_code, allow_local_builds
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


@tracing.trace()
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


@tracing.trace()
def transitive_reachability_filter(
    args: out.TransitiveReachabilityFilterParams,
) -> List[out.TransitiveFinding]:
    call = out.FunctionCall(out.CallTransitiveReachabilityFilter(args))
    ret: Optional[out.RetTransitiveReachabilityFilter] = rpc_call(
        call, out.RetTransitiveReachabilityFilter
    )
    if ret is None:
        # return the same findings
        return args.findings
    return ret.value


@tracing.trace()
def dump_rule_partitions(args: out.DumpRulePartitionsParams) -> bool:
    call = out.FunctionCall(out.CallDumpRulePartitions(args))
    ret: Optional[out.RetDumpRulePartitions] = rpc_call(call, out.RetDumpRulePartitions)
    if ret is None:
        logger.error("Failed to dump rule partitions")
        return out.RetDumpRulePartitions(False).value
    return ret.value


@tracing.trace()
def get_targets(scanning_roots: out.ScanningRoots) -> out.TargetDiscoveryResult:
    def summarize(desc: str, xs: list, threshold: int = 30) -> None:
        if len(xs) > 0:
            s = ", ".join([str(x) for x in xs[:threshold]])
            if len(xs) > threshold:
                s += f" (and {len(xs) - threshold} others)"
            logger.debug(f"get_targets resp: {desc}: {s}")

    logger.debug(f"get_targets request: {scanning_roots}")

    call = out.FunctionCall(out.CallGetTargets(scanning_roots))
    ret: Optional[out.RetGetTargets] = rpc_call(call, out.RetGetTargets)
    if ret is None:
        logger.error("Failed to obtain target files from semgrep-core")
        return out.TargetDiscoveryResult([], [], [])

    summarize("target paths", ret.value.target_paths)
    summarize("core errors", ret.value.errors)
    summarize("skipped targets", ret.value.skipped)

    return ret.value


@tracing.trace()
def match_subprojects(dependency_source_files: List[out.Fpath]) -> List[out.Subproject]:
    call = out.FunctionCall(out.CallMatchSubprojects(dependency_source_files))
    ret: Optional[out.RetMatchSubprojects] = rpc_call(call, out.RetMatchSubprojects)
    if ret is None:
        logger.error("Failed to match subprojects")
        return []
    return ret.value
