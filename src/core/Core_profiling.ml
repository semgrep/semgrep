(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Profiling info on rule matching and target parsing
 * Many of the types below are created to collect profiling information in as
 * well-typed a manner as possible. Creating a type for practically every
 * stage that reports matches is annoying, but prevents us from relying on
 * dummy values or unlabeled tuples.
 *
 * Another challenge we face is that the extra information can take a lot
 * of memory, which scales with both the number of rules and files. On
 * large repos, this is the most significant factor driving semgrep's
 * memory consumption. Therefore, if the skipped targets (for example) are
 * not being used, we don't want to save them. On the other hand, we want
 * to feel confident in the correctness of the code and make it easy to
 * know what is or isn't being saved. And we want our code to be relatively
 * readable.
 *
 * The debug_info type attempts to solve this. It has a variant for each
 * verbosity mode semgrep-core may be invoked with, which contains all
 * the fields semgrep requests from semgrep-core in that mode. Each result
 * contains debug_info in addition to the always-reported fields. The
 * variant of debug_info used in the results is always determined either
 * by the mode, which is a global set in Main.ml after the arguments are
 * read, or by a previous result. In this way we ensure that the fields
 * stored in the result are determined by the arguments passed by the user.
 *
 * Alternatives considered to the extra field:
 * - storing the information but just ommitting it in the final result (I
 *   tried this but it still uses too much memory)
 * - using option types within the result types for field and a global
 *   config to decide which fields are in use
 *
 * I considered the latter, but I'm not a fan of mix-and-match option types.
 * Maybe I just think it makes it tempting to use map_option, where here
 * the assumption that is being made feels more obvious. It also makes it
 * extra clear what information is being used for each mode and encourages
 * us to do the work of deciding what should be included instead of giving
 * the user choice that they probably don't know how to make intelligently.
 * That being said, if we wanted users to be able to "mix-and-match" request
 * fields, we should move to the option-types paradigm. Also, the collate
 * functions are quite ugly. I'm open to argument.
 *
 * Like for Core_error.ml and Core_result.ml, this "core" profiling
 * is eventually converted in semgrep_output_v1.core_timing and
 * which is then transformed in ProfilingData in profiling.py and
 * finally transformed in a semgrep_output_v1.profile type.
 * There's also Profiler.ml in osemgrep, and poor man profiler
 * stuff in libs/profiling/profiling.ml accessed via --profile
 * LATER: remove some intermediate types.
 *)

(*****************************************************************************)
(* Debug/Profile choice *)
(*****************************************************************************)
(* Options for what extra debugging information to output.
 * These are generally memory intensive fields that aren't strictly needed
 *)

(* coupling: the debug_info variant of each result record should always
 *  be the same as the mode's variant *)
type debug_mode = MDebug | MTime | MNo_info [@@deriving show]

type 'a debug_info =
  (* -debug: save all the information that could be useful *)
  | Debug of {
      skipped_targets : Semgrep_output_v1_t.skipped_target list;
      profiling : 'a;
    }
  (* -json_time: save just profiling info; currently our metrics record this *)
  | Time of { profiling : 'a }
  (* save nothing else *)
  | No_info
[@@deriving show]

let debug_info_to_option = function
  | Debug { profiling; _ } -> Some profiling
  | Time { profiling } -> Some profiling
  | No_info -> None

let mode = ref MNo_info

(*****************************************************************************)
(* Different formats for profiling info as we have access to more data *)
(*****************************************************************************)

(* general type *)
type times = { parse_time : float; match_time : float }

(* Save time information as we run each rule *)

type rule_profiling = {
  rule_id : Rule_ID.t;
  parse_time : float;
  match_time : float;
}
[@@deriving show]

(* Save time information as we run each file *)

type file_profiling = {
  file : Fpath.t;
  rule_times : rule_profiling list;
  run_time : float;
}
[@@deriving show]

type partial_profiling = { file : Fpath.t; rule_times : rule_profiling list }
[@@deriving show]

(* Result object for the entire rule *)

(* old: was called Report.final_profiling before *)
type t = {
  rules : Rule.rule list;
  rules_parse_time : float;
  file_times : file_profiling list;
  (* This is meant to represent the maximum amount of memory used by
     Semgrep during the course of its execution.

     This is useful to emit with the other profiling data for telemetry
     purposes, particuarly as it relates to measuring memory management
     with DeepSemgrep.

     It's not important that this number be incredibly precise, but
     measuring general trends is useful for ascertaining our memory
     usage.
  *)
  max_memory_bytes : int;
}
[@@deriving show]

(*****************************************************************************)
(* Create empty versions of profiling objects *)
(*****************************************************************************)

let empty_partial_profiling file = { file; rule_times = [] }

let empty_rule_profiling rule =
  { rule_id = fst rule.Rule.id; parse_time = 0.0; match_time = 0.0 }

let empty_extra profiling =
  match !mode with
  | MDebug -> Debug { skipped_targets = []; profiling }
  | MTime -> Time { profiling }
  | MNo_info -> No_info
