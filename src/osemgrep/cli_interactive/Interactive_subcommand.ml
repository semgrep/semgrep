(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-interactive command, execute it and exit.

*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type prompt = Exit | Pat of Xpattern.t * bool | Any | All
(*| Lparen
  | Rparen
*)

type interactive_pat =
  | P of Xpattern.t * bool
  | And of interactive_pat * interactive_pat
  | Or of interactive_pat * interactive_pat

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let prompt xlang =
  prerr_string "> ";
  flush stderr;
  let s = read_line () in
  match s with
  | "exit" -> Exit
  | "any" -> Any
  | "all" -> All
  (*| "(" -> Lparen
    | ")" -> Rparen
  *)
  | _ when String.starts_with ~prefix:"not " s ->
      let s = Str.string_after s 4 in
      (* TODO: error handle *)
      let lang = Xlang.to_lang_exn xlang in
      let lpat = lazy (Parse_pattern.parse_pattern lang s) in
      Pat
        ( Xpattern.mk_xpat
            (Xpattern.Sem (lpat, lang))
            (s, Tok.unsafe_fake_tok ""),
          false )
  | _else_ ->
      (* TODO: error handle *)
      let lang = Xlang.to_lang_exn xlang in
      let lpat = lazy (Parse_pattern.parse_pattern lang s) in
      Pat
        ( Xpattern.mk_xpat
            (Xpattern.Sem (lpat, lang))
            (s, Tok.unsafe_fake_tok ""),
          true )

let fk = Tok.unsafe_fake_tok ""

let rec translate_formula = function
  | P (pat, true) -> Rule.P pat
  | P (pat, false) -> Rule.Not (fk, P pat)
  | And (ipat1, ipat2) ->
      Rule.And
        ( fk,
          {
            conjuncts = [ translate_formula ipat1; translate_formula ipat2 ];
            conditions = [];
            focus = [];
          } )
  | Or (ipat1, ipat2) ->
      Rule.Or (fk, [ translate_formula ipat1; translate_formula ipat2 ])

let mk_fake_rule lang formula =
  {
    Rule.id = ("-i", fk);
    mode = `Search formula;
    (* alt: could put xpat.pstr for the message *)
    message = "";
    severity = Error;
    languages = lang;
    options = None;
    equivalences = None;
    fix = None;
    fix_regexp = None;
    paths = None;
    metadata = None;
  }

let report f xlang formula xtargets =
  match formula with
  | None -> failwith "bad"
  | Some formula ->
      let rule_formula = translate_formula formula in
      let fake_rule = mk_fake_rule xlang rule_formula in
      let hook _s (m : Pattern_match.t) =
        (*let content =
            m.tokens
            |> Lazy.force
            |> Common.map Tok.content_of_tok
            |> Matching_report.join_with_space_if_needed
          in
        *)
        f m Metavariable.ii_of_mval
      in
      let xconf =
        {
          Match_env.config = Rule_options.default_config;
          equivs = [];
          nested_formula = false;
          matching_explanations = false;
          filter_irrelevant_rules = false;
        }
      in
      let count = ref 0 in
      let res =
        xtargets
        |> Common.map (fun xtarget ->
               let results =
                 Match_search_mode.check_rule fake_rule hook xconf xtarget
               in
               count := !count + List.length results.matches;
               results)
      in
      Common.(pr2 (spf "Found %d total findings." !count));
      res

let check_interactive f xlang xtargets =
  let loop = ref true in
  let formula = ref None in
  let last_op = ref true in
  let handle_pat (pat, b) =
    match !formula with
    | None -> formula := Some (P (pat, b))
    | Some pat1 ->
        if !last_op then formula := Some (And (pat1, P (pat, b)))
        else formula := Some (Or (pat1, P (pat, b)))
  in
  while !loop do
    match prompt xlang with
    | Exit -> failwith "bye bye"
    | All -> last_op := true
    | Any -> last_op := false
    | Pat (pat, b) ->
        handle_pat (pat, b);
        ignore (report f xlang !formula xtargets);
        ()
  done;
  ()

let semgrep_with_interactive_mode (config : Runner_config.t) =
  (* TODO: support generic and regex patterns as well. See code in Deep.
   * Just use Parse_rule.parse_xpattern xlang (str, fk)
   *)
  let lang = Xlang.lang_of_opt_xlang_exn config.lang in

  (* copied from -e *)
  let roots = config.roots in
  let files, _skipped =
    Find_targets_old.files_of_dirs_or_files (Some lang) roots
  in
  let xlang = Xlang.L (lang, []) in
  let xtargets =
    files |> Common.map Fpath.to_string
    |> Common.map (Run_semgrep.xtarget_of_file config xlang)
  in
  check_interactive (Run_semgrep.print_match config) xlang xtargets

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (_conf : Interactive_CLI.conf) : Exit_code.t = Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Interactive_CLI.parse_argv argv in
  (* TODO: add CLI args to have config.roots, config.lang *)
  ignore semgrep_with_interactive_mode;
  run conf
