val annotate_pro_findings :
  Xtarget.t ->
  (Finding.t, Core_profiling.partial_profiling) Core_result.match_result ->
  (Finding.t, Core_profiling.partial_profiling) Core_result.match_result
