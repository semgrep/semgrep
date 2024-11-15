type conf = {
  merge_partial_results_dir : Fpath.t option;
  merge_partial_results_output : Fpath.t option;
  validate_partial_results_expected : Fpath.t option;
  validate_partial_results_actual : Fpath.t option;
}
[@@deriving show]

val hook_pro_read_and_merge_partial_scan_results :
  (input_dir:Fpath.t -> output_json:Fpath.t -> unit) option ref

val hook_pro_read_and_validate_partial_scan_results :
  (expected:Fpath.t -> actual:Fpath.t -> bool) option ref

val maybe_merge_partial_scan_results_then_exit : conf -> unit
val maybe_validate_partial_scan_results_then_exit : conf -> unit
