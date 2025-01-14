open Common
module MV = Metavariable
module OutJ = Semgrep_output_v1_j
module OutUtils = Semgrep_output_utils
module G = AST_generic

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type replacement = {
  mval_content : string Lazy.t;
  propagated_content : string option;
}

type replacement_ctx = (string * replacement) list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let metavar_string_of_any any =
  (* TODO: metavar_string_of_any is used in get_propagated_value
      to get the string for propagated values. Not all propagated
      values will have origintoks. For example, in
          x = 1; y = x + 1; ...
     we have y = 2 but there is no source location for 2.
     Handle such cases *)
  any |> AST_generic_helpers.ii_of_any
  |> List.filter Tok.is_origintok
  |> List.sort Tok.compare_pos
  |> List_.map Tok.content_of_tok
  |> Core_text_output.join_with_space_if_needed

let propagated_value_string_of_mval mval =
  let any = MV.mvalue_to_any mval in
  match any with
  | G.E { e = N (Id (_, id_info)); _ } -> (
      match !(id_info.id_svalue) with
      | Some (Lit x) ->
          let any = G.E (G.L x |> G.e) in
          Some (metavar_string_of_any any)
      | Some (Sym x) ->
          let any = G.E x in
          Some (metavar_string_of_any any)
      | Some (Cst _) -> None
      | Some NotCst -> None
      | None -> None)
  | __else__ -> None

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let of_bindings bindings =
  bindings
  |> List_.filter_map (fun (mvar, mval) ->
         match MV.range_of_mvalue mval with
         | None -> None
         | Some (file, mval_range) ->
             let mval_content = lazy (Range.content_at_range file mval_range) in
             let propagated_content = propagated_value_string_of_mval mval in
             Some (mvar, { mval_content; propagated_content }))

let of_out (metavars : OutJ.metavars) =
  metavars
  |> List_.map (fun (mvar, metavar_value) ->
         let mval_content = lazy metavar_value.OutJ.abstract_content in
         let propagated_content =
           Option.map
             (fun svalue -> svalue.OutJ.svalue_abstract_content)
             metavar_value.propagated_value
         in
         (mvar, { mval_content; propagated_content }))

let interpolate_metavars ?fmt (text : string) (ctx : replacement_ctx) : string =
  (* sort by metavariable length to avoid name collisions
   * (eg. $X2 must be handled before $X)
   *)
  let ctx =
    ctx
    |> List.sort (fun (a, _) (b, _) ->
           compare (String.length b) (String.length a))
  in
  ctx
  |> List.fold_left
       (fun text (mvar, { mval_content; propagated_content }) ->
         (* necessary typing to help the type check disambiguate fields,
          * because of the use of multiple fields with the same
          * name in semgrep_output_v1.atd *)

         (* first value($X), and then $X *)
         text
         |> Str.global_substitute
              (Str.regexp_string (spf "value(%s)" mvar))
              (fun _whole_str ->
                match propagated_content with
                | Some s -> s (* default to the matched value *)
                | None -> Lazy.force mval_content)
         |> Str.global_substitute (Str.regexp_string mvar) (fun _whole_str ->
                let mval_content = Lazy.force mval_content in
                Option.fold ~none:mval_content
                  ~some:(fun fmt -> fmt mval_content)
                  fmt))
       text
