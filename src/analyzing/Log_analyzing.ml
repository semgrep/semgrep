let src = Logs.Src.create "semgrep.analyzing"

module Log = (val Logs.src_log src : Logs.LOG)

(*****************************************************************************)
(* Debugging tags, select with SEMGREP_LOG_TAGS *)
(*****************************************************************************)

let svalue_tag = Logs_.create_tags [ "svalue" ]

(* Prints 'Deep_dataflow_util.uses_table'. *)
let fdeps_tag = Logs_.create_tags [ "fdeps" ]
