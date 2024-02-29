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

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* Check if the taint trace of the pattern crosses functions or files *)

let filename_of_token (tok : Tok.t) =
  match tok with
  | OriginTok loc -> Some loc.pos.file
  | FakeTok _ -> None
  | ExpandedTok (loc, _) -> Some loc.pos.file
  | Ab -> None

let tokens_of_trace_item (trace_item : Pattern_match.taint_trace_item) =
  let rec tokens_of_call_trace (call_trace : Pattern_match.taint_call_trace) =
    match call_trace with
    | Toks ts -> ts
    | Call { call_toks; intermediate_vars; call_trace } ->
        call_toks @ intermediate_vars @ tokens_of_call_trace call_trace
  in
  tokens_of_call_trace trace_item.source_trace
  @ tokens_of_call_trace trace_item.sink_trace
  @ trace_item.tokens

let is_interfile_trace (trace : Pattern_match.taint_trace) =
  let trace_tokens = trace |> List.concat_map tokens_of_trace_item in
  let token_filenames = trace_tokens |> List_.map_filter filename_of_token in
  (* iff two tokens have different filenames, the trace crosses files *)
  match token_filenames with
  | []
  | [ _ ] ->
      false
  | f :: fs -> not (List.for_all (fun x -> x = f) fs)

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
                   (is_interprocedural_trace trace, is_interfile_trace trace)
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
