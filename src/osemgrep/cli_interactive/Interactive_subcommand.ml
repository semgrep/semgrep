(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-interactive command, execute it and exit.

*)

open Notty
open Notty_unix

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type command = Exit | Pat of Xpattern.t * bool | Any | All
(*| Lparen
  | Rparen
*)

type interactive_pat =
  | IPat of Xpattern.t * bool
  | IAll of interactive_pat list
  | IAny of interactive_pat list

module Matches = struct
  type 'a t = {
    before_rev : 'a list;
    pointer : int;
    max_len : int;
    after : 'a list;
  }

  let shift_left m =
    match (m.before_rev, m.after) with
    | [], _ -> m
    | x :: xs, after -> { m with before_rev = xs; after = x :: after }

  let shift_right m =
    match (m.before_rev, m.after) with
    | _, [] -> m
    | before_rev, x :: xs -> { m with before_rev = x :: before_rev; after = xs }

  let move_ptr_left m =
    if m.pointer <= 0 then { (shift_left m) with pointer = 0 }
    else { m with pointer = m.pointer - 1 }

  let move_ptr_right m =
    if m.pointer >= m.max_len - 1 then
      { (shift_right m) with pointer = m.max_len - 1 }
    else { m with pointer = m.pointer + 1 }

  let rec _shift_left_n n m =
    if n <= 0 then m else _shift_left_n (n - 1) (shift_left m)

  let rec _shift_right_n n m =
    if n <= 0 then m else _shift_right_n (n - 1) (shift_right m)

  let take n m = Common2.take_safe n m.after
  let of_list max_len l = { before_rev = []; after = l; pointer = 0; max_len }
  let position m = m.pointer
end

(* The type of the state for the interactive loop.
   This is the information we need to carry in between every key press,
   and whenever we need to redraw the canvas.
*)
type state = {
  xlang : Xlang.t;
  xtargets : Xtarget.t list;
  input : string; (* TODO: remove *)
  matches : (string * Pattern_match.t list) Matches.t;
  cur_line_rev : char list;
      (** The current line that we are reading in, which is not yet
        finished.
        It's in reverse because we're consing on to the front.
      *)
  pat : interactive_pat option;
  mode : bool;  (** True if `All`, false if `Any`
      *)
  term : Term.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let files_height_of_term term = snd (Term.size term) - 4

let empty xlang xtargets term =
  {
    xlang;
    xtargets;
    input = "";
    matches =
      {
        before_rev = [];
        after = [];
        pointer = 0;
        max_len = files_height_of_term term;
      };
    cur_line_rev = [];
    pat = None;
    mode = true;
    term;
  }

let change_str s state = { state with input = s }
let cons_char c state = { state with cur_line_rev = c :: state.cur_line_rev }

let get_current_line state =
  Common2.string_of_chars (List.rev state.cur_line_rev)

let fk = Tok.unsafe_fake_tok ""

let rec translate_formula = function
  | IPat (pat, true) -> Rule.P pat
  | IPat (pat, false) -> Rule.Not (fk, P pat)
  | IAll ipats ->
      Rule.And
        ( fk,
          {
            conjuncts = List.map translate_formula ipats;
            conditions = [];
            focus = [];
          } )
  | IAny ipats -> Rule.Or (fk, List.map translate_formula ipats)

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

let matches_of_new_ipat new_ipat state =
  let rule_formula = translate_formula new_ipat in
  let fake_rule = mk_fake_rule state.xlang rule_formula in
  let hook _s (_m : Pattern_match.t) = () in
  let xconf =
    {
      Match_env.config = Rule_options.default_config;
      equivs = [];
      nested_formula = false;
      matching_explanations = false;
      filter_irrelevant_rules = false;
    }
  in
  let res =
    state.xtargets
    |> Common.map (fun xtarget ->
           let results =
             Match_search_mode.check_rule fake_rule hook xconf xtarget
           in
           results)
  in
  let res_by_file =
    state.xtargets
    |> Common.map (fun { Xtarget.file; _ } ->
           (file, Report.collate_rule_results file res))
    |> Common.map_filter (fun (file, (res : _ Report.match_result)) ->
           match res.matches with
           | [] -> None
           | _ -> Some (file, res.matches))
    |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
  in
  Matches.of_list (files_height_of_term state.term) res_by_file

(*****************************************************************************)
(* User Interface *)
(*****************************************************************************)

let render_screen state =
  let w, _h = Term.size state.term in
  (* Minus two, because one for the line, and one for
     the input line.
  *)
  let lines_of_files = files_height_of_term state.term in
  let lines_to_pad_below_to_reach l n =
    if List.length l >= n then 0 else n - List.length l
  in
  let matches =
    Matches.take lines_of_files state.matches
    |> Common.mapi (fun idx (file, _) ->
           if idx = Matches.position state.matches then
             I.string A.(fg (gray 19) ++ st bold ++ bg (gray 5)) file
           else I.string (A.fg (A.gray 16)) file)
  in
  let s = I.string A.empty state.input in
  let bar = String.make w (Char.chr 45) in
  let prompt =
    I.(
      matches |> I.vcat
      |> I.vpad 0 (lines_to_pad_below_to_reach matches lines_of_files)
      <-> string (A.fg (A.gray 12)) bar
      <-> (string (A.fg A.cyan) "> " <|> string A.empty (get_current_line state))
      <-> string (A.fg A.green) "Semgrep Interactive Mode")
  in
  I.(s <-> prompt)

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

let parse_command ({ xlang; _ } as state : state) =
  let s = get_current_line state in
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

let execute_command (state : state) =
  let cmd = parse_command state in
  let handle_pat (pat, b) =
    let new_pat = IPat (pat, b) in
    match (state.pat, state.mode) with
    | None, _ -> new_pat
    | Some (IAll pats), true -> IAll (new_pat :: pats)
    | Some (IAny pats), false -> IAny (new_pat :: pats)
    | Some pat, true -> IAny [ new_pat; pat ]
    | Some pat, false -> IAll [ new_pat; pat ]
  in
  let state =
    match cmd with
    | Exit -> failwith "bye bye"
    | All -> { state with mode = true }
    | Any -> { state with mode = false }
    | Pat (pat, b) ->
        let new_ipat = handle_pat (pat, b) in
        let matches = matches_of_new_ipat new_ipat state in
        { state with matches }
  in
  (* Remember to reset the current line after executing a command. *)
  { state with cur_line_rev = [] }

(*****************************************************************************)
(* Interactive loop *)
(*****************************************************************************)

let interactive_loop xlang xtargets =
  let rec update (t : Term.t) state =
    Term.image t (render_screen state);
    loop t state
  and loop t state =
    match Term.event t with
    | `Key (`Enter, _) ->
        let state = execute_command state in
        update t state
    | `Key (`Backspace, _) -> (
        match state.cur_line_rev with
        | [] -> loop t state
        | _ :: cs -> update t { state with cur_line_rev = cs })
    | `Key (`Arrow `Left, _) -> update t (change_str "left" state)
    | `Key (`Arrow `Right, _) -> update t (change_str "right" state)
    | `Key (`Arrow `Up, _) ->
        update t { state with matches = Matches.move_ptr_left state.matches }
    | `Key (`Arrow `Down, _) ->
        update t { state with matches = Matches.move_ptr_right state.matches }
    | `Key (`ASCII c, _) -> update t (cons_char c state)
    | `Resize _ -> update t state
    | _ -> loop t state
  in
  let t = Term.create () in
  let state = empty xlang xtargets t in
  (* TODO: change *)
  if true then update t state;
  Term.release t

(* TODO: we should rewrite this to use the osemgrep file targeting instead
 * of the deprecated (and possibly slow) files_of_dirs_or_files() below
 *)
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
  interactive_loop xlang xtargets

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Interactive_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.logging_level;
  let config = Core_runner.runner_config_of_conf conf.core_runner_conf in
  let config =
    {
      config with
      roots = conf.target_roots;
      lang = Some (Xlang.L (conf.lang, []));
    }
  in
  semgrep_with_interactive_mode config;
  Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Interactive_CLI.parse_argv argv in
  (* TODO: add CLI args to have config.roots, config.lang *)
  ignore semgrep_with_interactive_mode;
  run conf
