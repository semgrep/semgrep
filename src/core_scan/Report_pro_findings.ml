(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)
(** Support the reporting of pro findings

  * Users want to know when a finding could only be found by the pro engine
  * so that they can understand the value Semgrep provides. Additionally,
  * we want to surface our "coolest" findings early to make sure users see
  * them.

  * To support this, we have a different type for the engine kind for matches,
  * which includes a `PRO_SPECIFIC` option that reports when a finding could
  * only be found using the pro engine. This option also allows us to highlight
  * findings we think users should prioritize.
  *)

module FileSet = Set.Make (String)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Check if the taint trace of the pattern crosses files *)

let union3 a b c = FileSet.union a (FileSet.union b c)

let files_of_toks (ts : Pattern_match.pattern_match_tokens) : FileSet.t =
  ts
  |> List.fold_left
       (fun acc_files t ->
         match Tok.loc_of_tok t with
         | Ok { pos; _ } -> FileSet.add pos.file acc_files
         | Error _ -> acc_files)
       FileSet.empty

let files_of_trace_item (trace_item : Pattern_match.taint_trace_item) =
  let rec files_of_call_trace (call_trace : Pattern_match.taint_call_trace) =
    match call_trace with
    | Toks ts -> files_of_toks ts
    | Call { call_toks; intermediate_vars; call_trace } ->
        let intermediate_var =
          match intermediate_vars with
          | [] -> []
          | x :: _ -> [ x ]
        in
        union3 (files_of_toks call_toks)
          (files_of_toks intermediate_var)
          (files_of_call_trace call_trace)
  in
  union3
    (files_of_call_trace trace_item.source_trace)
    (files_of_call_trace trace_item.sink_trace)
    (files_of_toks trace_item.tokens)

let is_interfile_trace (trace : Pattern_match.taint_trace) =
  let files_in_trace =
    trace
    |> List.fold_left
         (fun acc_files trace_item ->
           FileSet.union acc_files (files_of_trace_item trace_item))
         FileSet.empty
  in
  FileSet.cardinal files_in_trace > 1

(* Check if the taint trace of the pattern crosses functions *)

let is_interprocedural_trace (trace : Pattern_match.taint_trace) =
  trace
  |> List.exists (fun (trace_item : Pattern_match.taint_trace_item) ->
         match (trace_item.source_trace, trace_item.sink_trace) with
         | Call _, _ -> true
         | _, Call _ -> true
         | Toks _, Toks _ -> false)

(*****************************************************************************)
(* Entrypoint *)
(*****************************************************************************)

let annotate_pro_findings (xtarget : Xtarget.t)
    (res : Core_profiling.partial_profiling Core_result.match_result) =
  {
    res with
    matches =
      res.matches
      |> List_.map (fun (x : Pattern_match.t) ->
             let proprietary_language = Xlang.is_proprietary xtarget.xlang in
             let interproc_taint, interfile_taint =
               match x.taint_trace with
               | None -> (false, false)
               | Some trace ->
                   let trace = Lazy.force trace in
                   let interfile_trace = is_interfile_trace trace in
                   (* All interfile taint findings are necessarily also interprocedural
                      taint findings, but the taint trace might not contain a Call node *)
                   ( interfile_trace || is_interprocedural_trace trace,
                     interfile_trace )
             in
             let engine_of_match : Engine_kind.engine_of_finding =
               if
                 not (proprietary_language || interproc_taint || interfile_taint)
               then `PRO
               else
                 `PRO_REQUIRED
                   { proprietary_language; interproc_taint; interfile_taint }
             in
             { x with engine_of_match });
  }
