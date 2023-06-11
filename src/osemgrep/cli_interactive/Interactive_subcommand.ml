(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Parse a semgrep-interactive command, execute it and exit.
 *
 * TODO:
 *  - data race on AST_utils.busy_with_equal (pad tried
 *    to use Lock_protected.t around it but ended up with
 *    some Mutex deadlock exn)
 *  - turbo/live mode with displaying incremental matches
 *    (not waiting until we have found all the matches)
 *  - code highlighting using Highlight_AST.ml in codemap/efuns
 *  - leverage multicore (switch to OCaml 5.0 or find a way to use
 *    Parmap like in Run_semgrep.map_targets)
 *  - support more complex iformula (where, inside, etc.)
 *  - readline-like input with history
 *  - completion on function names in the project! (or maybe put this in
 *    osemgrep-pro interactive)
 *)

open Notty
open Notty_unix

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

type command =
  (* ex: "foobar" *)
  | Pat of Xpattern.t * bool
  (* boolean composition of patterns *)
  | Any
  | All
  (* actual commands *)
  | Exit

(* interactive formula *)
type iformula =
  | IPat of Xpattern.t * bool
  (* false = negated *)
  | IAll of iformula list
  | IAny of iformula list

type matches_by_file = {
  file : string;
  matches : Pattern_match.t Framed_zipper.t;
      (** A zipper, because we want to be able to go back and forth
        through the matches in the file.
        Fortunately, a regular zipper is equivalent to a framed
        zipper with a frame size of 1.
      *)
}

(* Xtarget.t contains a lazy reference on the AST that is not thread-safe,
 * hence the use of Lock_protected.t
 *)
type xtarget = Xtarget.t Lock_protected.t

(* The type of the state for the interactive loop.
   This is the information we need to carry in between every key press,
   and whenever we need to redraw the canvas.
   coupling: if you add refs below, please update fresh_state() further below.
*)
type state = {
  xlang : Xlang.t;
  xtargets : xtarget list;
  file_zipper : matches_by_file Framed_zipper.t ref;
      (** ref because our matches might change at any time when the
          thread which is doing matches completes.
          The thread needs to be able to communicate the new file zipper
          back to the main thread.
        *)
  cur_line_rev : char list;
      (** The current line that we are reading in, which is not yet finished.
          It's in reverse because we're consing on to the front.
      *)
  formula : iformula option;
  (* True if in "All mode", false if "Any mode". This is changed
   * when the user type 'all' or 'any' in the prompt.
   *)
  mode : bool;
  term : Notty_unix.Term.t;
  turbo : bool;  (** Whether or not to do Turbo. *)
  (* To tell the spawned thread to stop because its computation is not
   * needed anymore because the user typed already something else.
   * Ideally we could just call Thread.kill from the main thread on
   * the id of the thread returned by Thread.create(), but the function
   * is actually deprecated because it's not the recommended way to do.
   * See https://fa.caml.narkive.com/kBWpAhJu/caml-list-the-best-way-to-circumvent-the-lack-of-thread-kill
   * alt:
   * - http://haesbaert.org/oslo/oslo/Oslo/index.html
   * - http://ocsigen.org/lwt/5.5.0/api/Lwt_preemptive
   *)
  should_continue_iterating_targets : bool ref;
}

(* Arbitrarily, let's just set the width of files to 40 chars. *)
let files_width = 40

(* color settings *)
let semgrep_green = A.rgb_888 ~r:58 ~g:212 ~b:167
let light_green = A.rgb_888 ~r:77 ~g:255 ~b:175
let neutral_yellow = A.rgb_888 ~r:255 ~g:255 ~b:161
let light_blue = A.rgb_888 ~r:51 ~g:129 ~b:255
let bg_file_selected = A.(bg (gray 5))
let bg_match = A.(bg (A.rgb_888 ~r:128 ~g:83 ~b:0))
let bg_match_position = A.(bg light_green ++ fg (gray 3))
let fg_line_num = A.(fg neutral_yellow)

(* This ref indicates whether or not the interactive loop should refresh.
   This can happen if a thread has completed its computation, and the state
   has been replenished with new matches. We don't want to constantly
   redraw the screen, so we only redraw when this condition is true.
*)
let should_refresh = ref false

(*****************************************************************************)
(* ASCII art *)
(*****************************************************************************)

(* All these images are annotated with their width, which is what was used
   with the ASCII image generator to generate them.

   For the Semgrep logos, width is the more important factor, so we use that
   as the primary constraint for resizing. For the ghosts, height is, so we
   constrain primarily by that metric.
*)

let default_screen_lines_100 =
  [
    "                 ((((((                         \
     (((((                         ((((((                ";
    "          ((((((((((((((((((((          %(((((((((((((((((((           \
     ((((((((((((((((((((         ";
    "     %((((((((((((((((((((((((((    (((((((((((((((((((((((((((   \
     #((((((((((((((((((((((((((       ";
    "   (((((((((((((((((((((((((((((( ((((((((((((((((((((((((((((((( \
     (((((((((((((((((((((((((((((#    ";
    "  (((((((((((%          (((((((&((((((((((((           \
     (((((((((((((((((((%          ((((((((((((   ";
    "&(((((((((#                ((((((((((((((                 \
     (((((((((((((#                ((((((((((& ";
    "(((((((((                    ((((((((((&                   \
     (((((((((((                    ((((((((( ";
    "(((((((((                      (((((((((                     \
     (((((((((                      ((((((((";
    "(((((((((                      (((((((((                     \
     (((((((((                      ((((((((";
    " (((((((((                    ((((((((((                     \
     (((((((((#                    (((((((((";
    " ((((((((((                  ((((((((((((                   \
     ((((((((((((                  ((((((((((";
    "  (((((((((((              ((((((((((((((((%             \
     (((((#(((((((((((              ((((((((((( ";
    "   %((((((((((((((####(((((((((((((( \
     ((((((((((((###((((((((((((%((((((((((((((####((((((((((((((   ";
    "     #((((((((((((((((((((((((((((& ((((((((((((((((((((((((((((( \
     #((((((((((((((((((((((((((((&    ";
    "        (((((((((((((((((((((((%       (((((((((((((((((((((((       \
     (((((((((((((((((((((((%       ";
    "             ((((((((((((((                %(((((((((((((%                \
     ((((((((((((((            ";
  ]

let default_screen_lines_65 =
  [
    "             ((((                ((((                (((#            ";
    "       ((((((((((((((((    ((((((((((((((((    ((((((((((((((((      ";
    "     ((((((((((((((((((( (((((((((((((((((((( (((((((((((((((((((    ";
    "   (((((((          ((((((((((          ((((((((((          (((((((  ";
    "   (((((#             ((((((              ((((((             ((((((  ";
    "  ((((((              ((((((              ((((((              (((((  ";
    "   ((((((             ((((((&            (((((((             ((((((  ";
    "   (((((((          ((((((((((          ((((((((((          ((((((   ";
    "     (((((((((((((((((((( (((((((((((((((((( ((((((((((((((((((((    ";
    "       &(((((((((((((((    ((((((((((((((((    (((((((((((((((       ";
  ]

let get_default_screen_lines width =
  if width > 105 then default_screen_lines_100
  else if width > 70 then default_screen_lines_65
  else []

(* about 40 lines tall *)
let ghost_lines_80 =
  [
    "                       .-=*#%%@@@@@@%%#*+-.                       ";
    "                   -+#@@@@@@@@@@@@@@@@@@@@@@%*-.                  ";
    "               .+%@@@@@@@@@%#**++++**#%@@@@@@@@@%+:               ";
    "             =%@@@@@@@*=:.               :=*%@@@@@@%=             ";
    "          .+@@@@@@#=.                        .=#@@@@@@*.          ";
    "         =@@@@@@+.                              .+@@@@@@+         ";
    "       :%@@@@@+                                    =%@@@@%-       ";
    "      +@@@@@+                                        +@@@@@*      ";
    "     #@@@@%:                                          .#@@@@%     ";
    "    #@@@@#                                              *@@@@%.   ";
    "   #@@@@#                                                *@@@@#   ";
    "  =@@@@%                                                  #@@@@+  ";
    "  @@@@@.                                                   @@@@@: ";
    " +@@@@+            ...                      ...            -@@@@# ";
    " @@@@@.         .*@@@@@#:                .*@@@@@#:          @@@@@.";
    ":@@@@%         .@@@@@@@@@:              .@@@@@@@@@:         *@@@@-";
    "-@@@@*         -@@@@@@@@@+              -@@@@@@@@@+         +@@@@+";
    "-@@@@*          *@@@@@@@%.               *@@@@@@@%.         =@@@@+";
    "-@@@@*           :+###+:                  :+###+:           =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*                                                      =@@@@+";
    "-@@@@*      :*#*-              =*#+.             .+#*-      =@@@@+";
    "-@@@@*    .*@@@@@*           .#@@@@@-           =@@@@@%-    =@@@@+";
    "-@@@@*  .+@@@@@@@@%-        =@@@@@@@@#        .#@@@@@@@@#:  =@@@@+";
    "-@@@@* +@@@@@%+@@@@@*     .#@@@@@#@@@@@=     -@@@@@##@@@@@*.=@@@@+";
    "-@@@@%@@@@@@=  :%@@@@%:  =@@@@@#  =@@@@@*   *@@@@@=  -%@@@@@%@@@@+";
    "-@@@@@@@@@*      *@@@@@+#@@@@@=    .%@@@@@=@@@@@#.     =@@@@@@@@@+";
    "-@@@@@@@*.        -%@@@@@@@@*.       +@@@@@@@@@=         +@@@@@@@+";
    "-@@@@@#:            *@@@@@@-          :%@@@@@%.           .*@@@@@+";
    "#@@#-               -%@@*.             +@@%+               :#@@#. ";
  ]

(* about 25 lines tall *)
let ghost_lines_50 =
  [
    "             .-+#%@@@@@@%#+=:             ";
    "          -*@@@@@%#****##@@@@@#=          ";
    "       .*@@@%+-.           :+%@@@*:       ";
    "     .*@@@+.                  .+@@@#.     ";
    "    -@@@+                        +@@@=    ";
    "   =@@%:                          .%@@*   ";
    "  =@@%.                             #@@+  ";
    " :@@@.                               %@@- ";
    " #@@=       --:            .--.      -@@% ";
    " @@@.     =@@@@%.         %@@@@+      @@@.";
    ":@@@      #@@@@@:        .@@@@@%      %@@-";
    ":@@@       =*#*:          .+##=       %@@-";
    ":@@@                                  %@@-";
    ":@@@                                  %@@-";
    ":@@@                                  %@@-";
    ":@@@                                  %@@-";
    ":@@@                                  %@@-";
    ":@@@                                  %@@-";
    ":@@@                                  %@@-";
    ":@@@    :=:         ==.        .=-    %@@-";
    ":@@@  :#@@@*      :%@@@=      +@@@%-  %@@-";
    ":@@@.*@@@#@@@:   *@@@%@@#.  .%@@#%@@#:%@@-";
    ":@@@@@@+  -%@@+:%@@*. +@@@-=@@@=  =@@@@@@-";
    ":@@@@*.     *@@@@@-    :%@@@@#.     +@@@@-";
    ".%@#:        -%@*        +@@+        .*@@:";
  ]

let get_ghost_lines height =
  (* The ghost lines are annotated with width, but we actually want
     to use height, because they're taller than they are wide.

     This means height is more likely to be a limiting factor than width.
  *)
  if height > 50 then ghost_lines_80
  else if height > 30 then ghost_lines_50
  else []

(*****************************************************************************)
(* UI Helpers *)
(*****************************************************************************)

let height_of_files term = snd (Term.size term) - 3
let width_of_files _term = files_width
let width_of_preview term = fst (Term.size term) - width_of_files term - 1

let init_state turbo xlang xtargets term =
  {
    xlang;
    xtargets;
    file_zipper = ref (Framed_zipper.empty_with_max_len (height_of_files term));
    should_continue_iterating_targets = ref true;
    cur_line_rev = [];
    formula = None;
    mode = true;
    term;
    turbo;
  }

let fresh_state old_state =
  (* generating fresh refs! otherwise they will be shared between
   * different spawned threads.
   * alt: we could use mutable fields instead of refs in state.
   * mutable fields are freshly generated when doing
   *      { state with other_field = xx },
   * but they are also confusing (because of this magic), so simpler
   * for now to be explicit by calling a clear fresh_state() function.
   *)
  {
    old_state with
    file_zipper = ref !(old_state.file_zipper);
    should_continue_iterating_targets = ref true;
  }

let get_current_line state =
  Common2.string_of_chars (List.rev state.cur_line_rev)

let safe_subtract x y =
  let res = x - y in
  if res < 0 then 0 else res

(*****************************************************************************)
(* Engine Helpers *)
(*****************************************************************************)

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

(*****************************************************************************)
(* Calling the Semgrep Engine *)
(*****************************************************************************)
(* Right now we call the engine via Match_search_mode.check_rule()
 * which is pretty down in the call stack compared to
 * Run_semgrep.semgrep_with_rules(). That means we lose some of the
 * features provided upper in the call stack such as:
 * - Parallelization in Run_semgrep.map_targets().
 *   However, maybe it's hard to reuse this parallelization
 *   and it's easier for now to use threads for the dynamic refresh of the UI.
 * - the -filter_irrelevant_rules default handling (a.k.a -fast)
 *   So we had to explicitely call is_relevant_rule below
 * - probably more.
 *
 * At the same time it's nice to go directly to the function
 * we need; it's doubtful we want to write interactively tainting rules,
 * so getting directly to Match_search_mode.check_rule() can be simpler.
 *)
let matches_of_new_iformula (new_iform : iformula) (state : state) :
    matches_by_file Framed_zipper.t =
  let rule_formula = translate_formula new_iform in
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
      (* set to true for the -fast optimization! *)
      filter_irrelevant_rules = true;
    }
  in
  let res : Report.rule_profiling Report.match_result list =
    state.xtargets
    |> Common.map_filter (fun xtarget_prot ->
           if !(state.should_continue_iterating_targets) then
             xtarget_prot
             |> Lock_protected.with_lock (fun xtarget ->
                    if
                      Match_rules.is_relevant_rule_for_xtarget fake_rule xconf
                        xtarget
                    then
                      (* Calling the engine! *)
                      Some
                        (Match_search_mode.check_rule fake_rule hook xconf
                           xtarget)
                    else None)
             (* the user typed something else; we're not needed anyore *)
           else raise Thread.Exit)
  in

  let res_by_file =
    res
    |> List.concat_map (fun ({ matches; _ } : _ Report.match_result) ->
           Common.map (fun (m : Pattern_match.t) -> (m.file, m)) matches)
    |> Common2.group_assoc_bykey_eff
    (* A framed zipper with a frame size of 1 is the same as a regular
       zipper.
    *)
    |> Common.map (fun (file, pms) ->
           let sorted_pms =
             List.sort
               (fun { Pattern_match.range_loc = l1, _; _ }
                    { Pattern_match.range_loc = l2, _; _ } ->
                 Int.compare l1.pos.charpos l2.pos.charpos)
               pms
           in
           { file; matches = Framed_zipper.of_list 1 sorted_pms })
    |> List.sort (fun { file = k1; _ } { file = k2; _ } -> String.compare k1 k2)
  in
  Framed_zipper.of_list (height_of_files state.term) res_by_file

let parse_pattern_opt s state =
  try
    let lang = Xlang.to_lang_exn state.xlang in
    let pat = Parse_pattern.parse_pattern lang s in
    Some
      (Xpattern.mk_xpat
         (Xpattern.Sem (lazy pat, lang))
         (s, Tok.unsafe_fake_tok ""))
  with
  | Parsing.Parse_error
  | Parsing_error.Lexical_error _ ->
      None

(*****************************************************************************)
(* User Interface (Preview Pane) *)
(*****************************************************************************)

(* Given the bounds of a highlighted range, does this index
   fall in or out of the highlighted range?
*)
let placement_wrt_bound (lb, rb) idx =
  match (lb, rb) with
  | None, None -> Common.Middle3 ()
  | Some lb, _ when idx < lb -> Left3 ()
  | _, Some rb when idx >= rb -> Right3 ()
  | __else__ -> Middle3 ()

(* Given the range of a match, we want to split a given line
 * into things that are in the match, or are not.
 *)
let split_line (t1 : Tok.location) (t2 : Tok.location) (row, line) =
  let end_line, end_col, _ = Tok.end_pos_of_loc t2 in
  if row < t1.pos.line then (line, "", "")
  else if row > t2.pos.line then (line, "", "")
  else
    let lb = if row = t1.pos.line then Some t1.pos.column else None in
    let rb = if row = end_line then Some end_col else None in
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

let preview_of_match { Pattern_match.range_loc = t1, t2; _ } file state =
  let lines = Common2.cat file in
  let start_line = t1.pos.line in
  let end_line = t2.pos.line in
  let max_height = height_of_files state.term in
  let match_height = end_line - start_line in
  (* We want the appropriate amount of lines that will fit within
     our terminal window.
     We also want the match to be relatively centered, however.
     Fortunately, the precise math doesn't matter too much. We
     take the height of the match and try to equivalently
     pad it on both sides with other lines.
     TODO(brandon): cases for if the match is too close to the top
     or bottom of the file
  *)
  let preview_start, preview_end =
    if match_height <= max_height then
      (* if this fits within our window *)
      let extend_before = (max_height - match_height) / 2 in
      let start = safe_subtract start_line extend_before in
      (start, start + max_height)
    else (start_line, start_line + max_height)
  in
  let line_num_imgs, line_imgs =
    lines
    (* Row is 1-indexed *)
    |> Common.mapi (fun idx x -> (idx + 1, x))
    (* Get only the lines that we care about (the ones in the preview) *)
    |> Common.map_filter (fun (idx, line) ->
           if preview_start <= idx && idx < preview_end then
             Some (idx, split_line t1 t2 (idx, line))
           else None)
    (* Turn line numbers and the line contents to images *)
    |> Common.map (fun (idx, (l, m, r)) ->
           ( I.(string fg_line_num (Int.to_string idx)),
             I.(
               string A.empty l
               (* alt: A.(bg (rgb_888 ~r:255 ~g:255 ~b:194)) *)
               <|> string A.(st bold ++ bg_match) m
               <|> string A.empty r) ))
    |> Common2.unzip
  in
  (* Right-align the images and pad on the right by 1 *)
  let line_num_imgs_aligned_and_padded =
    let max_line_num_len =
      line_num_imgs
      |> Common.map (fun line_num_img -> I.width line_num_img)
      |> List.fold_left Int.max 0
    in
    line_num_imgs
    |> Common.map (fun line_num_img ->
           I.hsnap ~align:`Right max_line_num_len line_num_img)
    |> I.vcat |> I.hpad 0 1
  in
  (* Put the line numbers and contents together! *)
  I.(line_num_imgs_aligned_and_padded <|> vcat line_imgs)

let default_screen_img s state =
  let w = width_of_preview state.term in
  I.(
    (get_default_screen_lines w |> Common.map (I.string (A.fg semgrep_green)))
    @ [
        vpad 1 0
          (string A.(fg semgrep_green ++ st bold) "Semgrep Interactive Mode");
        vpad 1 1 (string A.empty "powered by Semgrep Open-Source Engine");
        string A.empty s;
      ]
    |> Common.map (hsnap w)
    |> vcat
    |> I.vsnap (height_of_files state.term))

let no_matches_found_img state =
  let h = height_of_files state.term in
  I.(
    (get_ghost_lines h |> Common.map (I.string (A.fg light_blue)))
    @ [ vpad 2 0 (string A.empty "no matches found") ]
    |> Common.map (hsnap (width_of_preview state.term))
    |> vcat |> I.vsnap h)

let render_preview_no_matches ~has_changed state =
  if state.turbo then
    (* In Turbo Mode, what we display here is dependent on two
        things, the current state of the buffer and whether we
        actually changed the query bar with our last key press.
    *)
    if String.equal (get_current_line state) "" then
      default_screen_img "(type a pattern to get started!)" state
    else if has_changed then default_screen_img "thinking..." state
    else no_matches_found_img state
  else if
    (* In regular mode, we don't care about those things, but we
        do care about whether we have entered a pattern or not.
    *)
    Option.is_some state.formula
  then no_matches_found_img state
  else default_screen_img "(type a pattern to get started!)" state

(*****************************************************************************)
(* User Interface (Top Left Pane) *)
(*****************************************************************************)

(* This just pretty-prints out the patterns we currently have in
   our tree.
*)
let render_patterns iformula =
  let rec loop = function
    | IPat ({ Xpattern.pstr = s, _; _ }, b) ->
        if b then I.(string A.empty (Common.spf "%s" s))
        else I.(string A.(fg lightblue) "not: " <|> string A.empty s)
    | IAll pats ->
        I.(
          let patterns =
            pats |> Common.map loop
            |> Common.map (fun img -> I.(string A.empty "- " <|> img))
            |> vcat |> hpad 2 0
          in
          hsnap (I.width patterns) ~align:`Left (string A.(fg lightblue) "all:")
          <-> patterns)
    | IAny pats ->
        I.(
          let patterns =
            pats |> Common.map loop
            |> Common.map (fun img -> I.(string A.empty "- " <|> img))
            |> vcat |> hpad 2 0
          in
          hsnap (I.width patterns) ~align:`Left (string A.(fg lightblue) "any:")
          <-> patterns)
  in
  match iformula with
  | IPat (_, true) -> I.(string A.(fg lightblue) "pattern: " <|> loop iformula)
  | _ -> loop iformula

(* This differs based on our mode. In Turbo Mode, this is just the
   list of files.

   In Normal Mode, we allow cumulative building of patterns, so this can
   sometimes print out the patterns that we are currently building up.
*)
let render_top_left_pane file_zipper state =
  let lines_to_pad_below_to_reach l n =
    if List.length l >= n then 0 else n - List.length l
  in
  let patterns =
    match state.formula with
    | None -> I.void 0 0
    | Some formula ->
        render_patterns formula
        |> I.hsnap (width_of_files state.term) ~align:`Left
  in
  let intermediary_bar =
    String.make (width_of_files state.term) '-' |> I.string (A.fg (A.gray 12))
  in
  let lines_of_files = height_of_files state.term - I.height patterns - 1 in
  let files =
    Framed_zipper.take lines_of_files file_zipper
    |> Common.mapi (fun idx { file; _ } ->
           if idx = Framed_zipper.relative_position file_zipper then
             I.string A.(fg (gray 19) ++ st bold ++ bg_file_selected) file
           else I.string (A.fg (A.gray 16)) file)
  in
  I.(
    files |> I.vcat
    |> I.vpad 0 (lines_to_pad_below_to_reach files lines_of_files)
    <-> intermediary_bar <-> patterns)

(*****************************************************************************)
(* User Interface (Screen) *)
(*****************************************************************************)

let render_screen ?(has_changed = false) state =
  let w, _h = Term.size state.term in
  (* Minus two, because one for the line, and one for
     the input line.
  *)
  (* The zipper shouldn't be able to change while we're
     rendering the screen, so it's safe to do this. This
     ensures that we don't act like it does, anyways.
  *)
  let file_zipper = !(state.file_zipper) in
  let top_left_pane = render_top_left_pane file_zipper state in
  let preview_pane =
    if Framed_zipper.is_empty file_zipper then
      render_preview_no_matches ~has_changed state
    else
      let { file; matches = matches_zipper } =
        Framed_zipper.get_current file_zipper
      in
      (* 1 indexed *)
      let match_idx = Framed_zipper.absolute_position matches_zipper + 1 in
      let total_matches = Framed_zipper.length matches_zipper in
      let match_position_img =
        if total_matches = 1 then I.void 0 0
        else
          I.string bg_match_position
            (Common.spf "%d/%d" match_idx total_matches)
          |> I.hsnap ~align:`Right (w - files_width - 1)
      in
      let pm = Framed_zipper.get_current matches_zipper in
      I.(match_position_img </> preview_of_match pm file state)
  in
  let vertical_bar = I.char A.empty '|' 1 (height_of_files state.term) in
  let horizontal_bar = String.make w '-' |> I.string (A.fg (A.gray 12)) in
  let mode =
    if state.turbo then I.void 0 0
    else if state.mode then I.(string A.(fg semgrep_green) "[ALL]")
    else I.(string A.(fg semgrep_green) "[ANY]")
  in
  let prompt =
    I.(
      mode
      <|> string (A.fg A.cyan) "> "
      <|> string A.empty (get_current_line state))
  in
  let lowerbar =
    let status =
      if state.turbo then "Semgrep Interactive Mode (TURBO ACTIVATED)"
      else "Semgrep Interactive Mode"
    in
    I.(string (A.fg A.green) status)
  in
  (* The format of the Interactive Mode UI is:
   *
   * files files vertical bar preview preview preview
   * files files vertical bar preview preview preview
   * files files vertical bar preview preview preview
   * files files vertical bar preview preview preview
   * files files vertical bar preview preview preview
   * horizontal bar   horizontal bar   horizontal bar
   * prompt prompt prompt prompt prompt prompt prompt
   * lower bar lower bar lower bar lower bar lower bar
   *)
  I.(
    top_left_pane
    (* THINK: unnecessary? *)
    |> (fun img -> I.hcrop 0 (I.width img - files_width) img)
    <|> vertical_bar <|> preview_pane <-> horizontal_bar <-> prompt <-> lowerbar)

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
    if state.turbo then new_pat
    else
      match (state.formula, state.mode) with
      | None, _ -> new_pat
      | Some (IAll pats), true -> IAll (new_pat :: pats)
      | Some (IAny pats), false -> IAny (new_pat :: pats)
      | Some pat, true -> IAll [ new_pat; pat ]
      | Some pat, false -> IAny [ new_pat; pat ]
  in
  let state =
    match cmd with
    | Exit -> failwith "bye bye"
    | All -> { state with mode = true }
    | Any -> { state with mode = false }
    | Pat (pat, b) ->
        let new_iformula = handle_pat (pat, b) in
        let file_zipper = matches_of_new_iformula new_iformula state in
        state.file_zipper := file_zipper;
        { state with formula = Some new_iformula }
  in
  (* Remember to reset the current line after executing a command,
     but only if we're not doing a Turbo run!
  *)
  if not state.turbo then { state with cur_line_rev = [] } else state

(*****************************************************************************)
(* Turbo mode *)
(*****************************************************************************)

(* This function spawns a thread for every single unique change to the current
   line. Each of these threads is responsible for populatin the `should_refresh`
   and `file_zipper` fields with updated entries, which is then consumed by
   the interactive loop as it notices.

   Care must be taken to ensure multiple threads don't mess with each other.
*)
let spawn_thread_if_turbo state =
  if state.turbo then
    Thread.create
      (fun _ ->
        let cur_line = get_current_line state in
        let pat_opt = parse_pattern_opt cur_line state in
        match (cur_line, pat_opt) with
        | "", _ ->
            (* When we go back to the empty line, reset the view to default. *)
            should_refresh := true;
            state.file_zipper :=
              Framed_zipper.empty_with_max_len (height_of_files state.term)
        | _, None -> ()
        | _, Some pat ->
            let new_iformula = IPat (pat, true) in
            let file_zipper = matches_of_new_iformula new_iformula state in
            state.file_zipper := file_zipper;
            should_refresh := true)
      ()
    |> ignore

let stop_thread_if_turbo state =
  if state.turbo then state.should_continue_iterating_targets := false

(*****************************************************************************)
(* Interactive loop *)
(*****************************************************************************)

let interactive_loop ~turbo xlang xtargets =
  let rec render_and_loop ?(has_changed = false) (t : Term.t) state =
    Term.image t (render_screen ~has_changed state);
    loop t state
  and loop t state =
    if !should_refresh then (
      (* If this is true, this indicates that we should refresh, because a
         thread has asynchronously given us new matches (and a new file zipper)
      *)
      should_refresh := false;
      render_and_loop t state)
    else
      (* I (Brandon) have absolutely no idea why, but for some reason
         using `Term.pending` here just loops forever and
         seems to think that it's always not pending.

         if not (Term.pending t) then

         ^ loops forever to this case

         So instead we just poll stdin to see whether there is
         data, which should be a good proxy for whether or not it
         is safe to proceed to the blocking `event` call.

         TODO(pad): we should not need any Unix.select, and no 0.0005 timeout.
         Thread should work fine with Term.event
      *)
      match Unix.select [ Unix.stdin ] [] [] 0.0005 with
      | [], _, _ ->
          (* stdin not available for reading, no data so let's cycle again
         *)
          loop t state
      | _ :: _, _, _ -> (
          match Term.event t with
          | `Key (`Enter, _) ->
              let state = execute_command state in
              render_and_loop t state
          | `Key (`Backspace, _) -> (
              match state.cur_line_rev with
              (* nothing to delete, just stay the same *)
              | [] -> loop t state
              | _ :: cs ->
                  stop_thread_if_turbo state;
                  let state = fresh_state { state with cur_line_rev = cs } in
                  spawn_thread_if_turbo state;
                  render_and_loop ~has_changed:true t state)
          | `Key (`ASCII c, _) ->
              stop_thread_if_turbo state;
              let state =
                fresh_state
                  { state with cur_line_rev = c :: state.cur_line_rev }
              in
              spawn_thread_if_turbo state;
              render_and_loop ~has_changed:true t state
          | `Key (`Arrow `Left, _) ->
              let file_zipper = !(state.file_zipper) in
              state.file_zipper :=
                Framed_zipper.map_current
                  (fun { file; matches = mz } ->
                    { file; matches = Framed_zipper.move_left mz })
                  file_zipper;
              render_and_loop t state
          | `Key (`Arrow `Right, _) ->
              let file_zipper = !(state.file_zipper) in
              state.file_zipper :=
                Framed_zipper.map_current
                  (fun { file; matches = mz } ->
                    { file; matches = Framed_zipper.move_right mz })
                  file_zipper;
              render_and_loop t state
          | `Key (`Arrow `Up, _) ->
              let file_zipper = !(state.file_zipper) in
              state.file_zipper := Framed_zipper.move_left file_zipper;
              render_and_loop t state
          | `Key (`Arrow `Down, _) ->
              let file_zipper = !(state.file_zipper) in
              state.file_zipper := Framed_zipper.move_right file_zipper;
              render_and_loop t state
          | `Resize _ -> render_and_loop t state
          | __else__ -> render_and_loop t state)
  in
  let t = Term.create () in
  Common.finalize
    (fun () ->
      let state = init_state turbo xlang xtargets t in
      (* fake if to shutdown warning 21 of ocamlc "nonreturn-statement" *)
      if true then render_and_loop state.term state)
    (fun () -> Term.release t)
  [@@profiling]

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

(* All the business logic after command-line parsing. Return the desired
   exit code. *)
let run (conf : Interactive_CLI.conf) : Exit_code.t =
  CLI_common.setup_logging ~force_color:false ~level:conf.logging_level;
  let targets, _skipped =
    Find_targets.get_targets conf.targeting_conf conf.target_roots
  in
  (* TODO: support generic and regex patterns as well. See code in Deep.
   * Just use Parse_rule.parse_xpattern xlang (str, fk)
   *)
  let xlang = Xlang.L (conf.lang, []) in
  let targets =
    targets |> List.filter (Filter_target.filter_target_for_xlang xlang)
  in
  let config = Core_runner.runner_config_of_conf conf.core_runner_conf in
  let config = { config with roots = conf.target_roots; lang = Some xlang } in
  let xtargets =
    targets |> Common.map Fpath.to_string
    |> Common.map (fun file ->
           let xtarget = Run_semgrep.xtarget_of_file config xlang file in
           Lock_protected.protect xtarget)
  in
  interactive_loop ~turbo:conf.turbo xlang xtargets;
  Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let main (argv : string array) : Exit_code.t =
  let conf = Interactive_CLI.parse_argv argv in
  run conf
