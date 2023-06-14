open File.Operators
module Out = Output_from_core_j

let logger = Logging.get_logger [ __MODULE__ ]

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* One-off experiment for Raja.
 *
 * This experiment consists in returning the enclosing function name
 * and function body in the extra_extra JSON field for each match
 * results. This code path is triggered when passing --core-opts='-raja'
 * to the Semgrep CLI.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let find_function_info (range_offsets : int * int)
    (ranges : Function_range.ranges) : Function_range.function_info option =
  let start, end_ = range_offsets in
  ranges
  |> List.find_opt (fun { Function_range.range = t1, t2; _ } ->
         let offset1 = Tok.bytepos_of_tok t1 in
         let offset2 = Tok.bytepos_of_tok t2 in
         start >= offset1 && end_ <= offset2)

(*****************************************************************************)
(* Cache *)
(*****************************************************************************)
let (cache : (Fpath.t, Function_range.ranges) Hashtbl.t) = Hashtbl.create 101

let ranges_of_path (path : Fpath.t) : Function_range.ranges =
  match Hashtbl.find_opt cache path with
  | Some x -> x
  | None ->
      let ast =
        (* ugly: parse_program may raise an exn when we can't infer
         * the language from the filename.
         * TODO: we should use just_parse_with_lang and get the language
         * from the languages: field in the rule corresponding to
         * the rule_id of the match. That would require to pass
         * more info to ranges_of_path() though.
         *)
        try Parse_target.parse_program !!path with
        | Failure _ -> []
      in
      let ranges = Function_range.ranges ast in
      Hashtbl.add cache path ranges;
      ranges

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let adjust_core_match_results (x : Out.core_match_results) :
    Out.core_match_results =
  let matches =
    x.matches
    |> Common.map (fun (m : Out.core_match) ->
           let _rule_id = m.rule_id in
           let path = Fpath.v m.location.path in
           let ranges = ranges_of_path path in
           let start, end_ =
             (m.location.start.offset, m.location.end_.offset)
           in
           match find_function_info (start, end_) ranges with
           | None ->
               logger#info "no function info found for range: %d, %d" start end_;
               m
           | Some { Function_range.name; range = t1, t2 } ->
               let offset1 = Tok.bytepos_of_tok t1 in
               let offset2 = Tok.bytepos_of_tok t2 in
               let extra =
                 {
                   m.extra with
                   extra_extra =
                     Some
                       (`Assoc
                         [
                           ("function_name", `String name);
                           ( "function_range",
                             `Assoc
                               [
                                 ("start", `Int offset1); ("end", `Int offset2);
                               ] );
                         ]);
                 }
               in
               { m with extra })
  in
  { x with matches }
