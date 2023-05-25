(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
   Parse a semgrep-interactive command, execute it and exit.

*)

open Notty
open Notty_unix

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type command = Exit | Pat of Xpattern.t * bool | Any | All

type interactive_pat =
  | IPat of Xpattern.t * bool
  | IAll of interactive_pat list
  | IAny of interactive_pat list

(* The type of the state for the interactive loop.
   This is the information we need to carry in between every key press,
   and whenever we need to redraw the canvas.
*)
type state = {
  xlang : Xlang.t;
  xtargets : Xtarget.t list;
  matches : (string * Pattern_match.t list) Pointed_zipper.t;
  cur_line_rev : char list;
      (** The current line that we are reading in, which is not yet
        finished.
        It's in reverse because we're consing on to the front.
      *)
  pat : interactive_pat option;
  mode : bool;  (** True if `All`, false if `Any`
      *)
  term : Notty_unix.Term.t;
}

(* Arbitrarily, let's just set the width of files to 40 chars. *)
let files_width = 40

(* color settings *)
let bg_file_selected = A.(bg (gray 5))
let bg_match = A.(bg (gray 5))

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let files_height_of_term term = snd (Term.size term) - 3

let empty xlang xtargets term =
  {
    xlang;
    xtargets;
    matches = Pointed_zipper.empty_with_max_len (files_height_of_term term);
    cur_line_rev = [];
    pat = None;
    mode = true;
    term;
  }

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
            conjuncts = Common.map translate_formula ipats;
            conditions = [];
            focus = [];
          } )
  | IAny ipats -> Rule.Or (fk, Common.map translate_formula ipats)

let mk_fake_rule lang formula =
  {
    Rule.id = (Rule.ID.of_string "-i", fk);
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
  let fake_rule =
    mk_fake_rule (Rule.languages_of_xlang state.xlang) rule_formula
  in
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
  let res : Report.rule_profiling Report.match_result list =
    state.xtargets
    |> Common.map (fun xtarget ->
           let results =
             Match_search_mode.check_rule fake_rule hook xconf xtarget
           in
           results)
  in
  let res_by_file =
    res
    |> List.concat_map (fun ({ matches; _ } : _ Report.match_result) ->
           Common.map (fun (m : Pattern_match.t) -> (m.file, m)) matches)
    |> Common2.group_assoc_bykey_eff
    |> List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2)
  in
  Pointed_zipper.of_list (files_height_of_term state.term) res_by_file

let safe_subtract x y =
  let res = x - y in
  if res < 0 then 0 else res

(*****************************************************************************)
(* User Interface *)
(*****************************************************************************)

(* Given the bounds of a highlighted range, does this index
   fall in or out of the highlighted range?
*)
let placement_wrt_bound (lb, rb) idx =
  match (lb, rb) with
  | None, None -> Common.Middle3 ()
  | Some lb, _ when idx < lb -> Left3 ()
  | _, Some rb when idx > rb -> Right3 ()
  | __else__ -> Middle3 ()

(* Given a range of locations, we want to split a given line
 * into things that are in the match, or are not.
 *)
let split_line (t1 : Tok.location) (t2 : Tok.location) (row, line) =
  if row < t1.pos.line then (line, "", "")
  else if row > t2.pos.line then (line, "", "")
  else
    let lb = if row = t1.pos.line then Some t1.pos.column else None in
    let rb = if row = t2.pos.line then Some t2.pos.column else None in
    let l_rev, m_rev, r_rev, _ =
      String.fold_left
        (fun (l, m, r, i) c ->
          match placement_wrt_bound (lb, rb) i with
          | Common.Left3 _ -> (c :: l, m, r, i + 1)
          | Middle3 _ -> (l, c :: m, r, i + 1)
          | Right3 _ -> (l, m, c :: r, i + 1))
        ([], [], [], 0) line
    in
    ( Common2.string_of_chars (List.rev l_rev),
      Common2.string_of_chars (List.rev m_rev),
      Common2.string_of_chars (List.rev r_rev) )

let img_of_match { Pattern_match.range_loc = t1, t2; _ } file state =
  let lines = Common2.cat file in
  let start_line = t1.pos.line in
  let end_line = t2.pos.line in
  let max_len = files_height_of_term state.term in
  let range_height = end_line - start_line in
  let preview_start, preview_end =
    if range_height <= max_len then
      (* if this fits within our window *)
      let extend_before = (max_len - range_height) / 2 in
      let start = safe_subtract start_line extend_before in
      (start, start + max_len)
    else (start_line, start_line + max_len)
  in
  lines
  |> Common.mapi (fun idx x -> (idx + 1, x))
  |> Common.map_filter (fun (idx, line) ->
         if preview_start <= idx && idx < preview_end then
           Some (split_line t1 t2 (idx, line))
         else None)
  |> Common.map (fun (l, m, r) ->
         I.(
           string A.empty l
           (* alt: A.(bg (rgb_888 ~r:255 ~g:255 ~b:194)) *)
           <|> string A.(st bold ++ bg_match) m
           <|> string A.empty r))
  |> I.vcat

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
    Pointed_zipper.take lines_of_files state.matches
    |> Common.mapi (fun idx (file, _) ->
           if idx = Pointed_zipper.position state.matches then
             I.string A.(fg (gray 19) ++ st bold ++ bg_file_selected) file
           else I.string (A.fg (A.gray 16)) file)
  in
  let preview =
    if Pointed_zipper.is_empty state.matches then
      I.string A.empty "preview unavailable (no matches)"
    else
      let file, pms = Pointed_zipper.get_current state.matches in
      match pms with
      | [] -> I.string A.empty "preview unavailable (impossible?)"
      | pm :: _ -> (
          (* THINK: is this try still relevant? *)
          try img_of_match pm file state with
          | _ -> I.string A.empty "preview unavailble")
  in
  let horizontal_bar = String.make w '-' |> I.string (A.fg (A.gray 12)) in
  let vertical_bar =
    I.char A.empty '|' 1 (files_height_of_term state.term) |> I.hpad 1 1
  in
  let prompt =
    I.(string (A.fg A.cyan) "> " <|> string A.empty (get_current_line state))
  in
  let lowerbar = I.(string (A.fg A.green) "Semgrep Interactive Mode") in
  I.(
    matches |> I.vcat
    (* THINK: unnecessary? *)
    |> I.vpad 0 (lines_to_pad_below_to_reach matches lines_of_files)
    |> (fun img -> I.hcrop 0 (I.width img - files_width) img)
    <|> vertical_bar <|> preview <-> horizontal_bar <-> prompt <-> lowerbar)

(*****************************************************************************)
(* Commands *)
(*****************************************************************************)

let parse_command ({ xlang; _ } as state : state) =
  let s = get_current_line state in
  match s with
  | "exit" -> Exit
  | "any" -> Any
  | "all" -> All
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
    | `Key (`Arrow `Left, _)
    | `Key (`Arrow `Right, _) ->
        update t state (* TODO *)
    | `Key (`Arrow `Up, _) ->
        update t { state with matches = Pointed_zipper.move_left state.matches }
    | `Key (`Arrow `Down, _) ->
        update t
          { state with matches = Pointed_zipper.move_right state.matches }
    | `Key (`ASCII c, _) ->
        update t { state with cur_line_rev = c :: state.cur_line_rev }
    | `Resize _ -> update t state
    | __else__ -> loop t state
  in
  let t = Term.create () in
  Common.finalize
    (fun () ->
      let state = empty xlang xtargets t in
      (* TODO: change *)
      if true then update t state)
    (fun () -> Term.release t)

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
  run conf
