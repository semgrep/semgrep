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

let _logger = Logging.get_logger [ __MODULE__ ]

[@@@warning "-26-27-32"]
(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type command =
  (* ex: "foobar" *)
  | Pat of Xpattern.t * bool
  (* boolean composition of patterns *)
  | Any
  | All
  (* actual commands *)
  | Exit

(* interactive formula, but in a zipper!

   When in Pattern Builder mode, we want to be able to traverse the
   formula, up and down. This means we need another zipper.

   The difficulty is that we would also like to be able to have the
   zipper stop on the `all` and `any` themselves, which means we need
   to have an extra entry in the zipper. We'll call that `Header`,
   described below.
*)
type iformula_zipper =
  | IPat of Xpattern.t * bool
  (* false = negated *)
  | Header
  (* Header should only be within a zipper of IAll or IAny (not top level),
     and only once. This is so if we have
     all:
       - foo
       - bar
     then "foo" and "bar" should be in the zipper, so we can go between
     them, but we might also want to be able to go to `all`, so we include
     a "header" as the first item in the zipper.
  *)
  | IAll of iformula_zipper Zipper.t
  | IAny of iformula_zipper Zipper.t

type matches_by_file = {
  file : string;
  matches : Pattern_match.t Zipper.t;
      (** A zipper, because we want to be able to go back and forth
        through the matches in the file.
      *)
}

(* Xtarget.t contains a lazy reference on the AST that is not thread-safe,
 * hence the use of Lock_protected.t
 *)
type xtarget = Xtarget.t Lock_protected.t

(* The kind of menu we are currently highlighting. We can be using the
   arrow keys to move around in the files, or the pattern building
   menu.

   In Navigator Mode, the arrow keys are used to move around in either
   the list of files, or the matches within a particular file. Editing
   is used to add new patterns to the pattern tree.

   In Pattern Builder Mode, the arrow keys are used to move around in
   the already-built pattern tree, and editing causes the pattern tree
   to be edited in-place.
*)
type menu_mode = Navigator | Pattern

(* The type of the state for the interactive loop.
   This is the information we need to carry in between every key press,
   and whenever we need to redraw the canvas.
   coupling: if you add refs below, please update fresh_state() further below.
*)
type state = {
  xlang : Xlang.t;
  xtargets : xtarget list;
  file_zipper : matches_by_file Framed_zipper.t ref Lock_protected.t;
      (** ref because our matches might change at any time when the
          thread which is doing matches completes.
          The thread needs to be able to communicate the new file zipper
          back to the main thread.
          Lock protected, because in Turbo mode, the turbo thread writes to
          this ref in order to buffer more matches into it, and the REPL thread
          writes to this ref in order to move up and down and stuff.
          We need to ensure these writes do not clash.
        *)
  cur_line_rev : char list;
      (** The current line that we are reading in, which is not yet finished.
          It's in reverse because we're consing on to the front.
      *)
  formula : iformula_zipper option;
  menu_mode : menu_mode;  (** The kind of menu we are currently highlighting. *)
  pattern_mode : bool;
      (** True if in "All mode", false if "Any mode". This is changed
        * when the user type 'all' or 'any' in the prompt.
        *)
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

type event =
  | End
  | Key of Unescape.key
  | Mouse of Unescape.mouse
  | Paste of Unescape.paste
  | Resize of { cols : int; rows : int }

(*****************************************************************************)
(* Constants *)
(*****************************************************************************)

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
let event_queue : event Queue.t = Queue.create ()

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

let height_of_preview term = snd (Term.size term) - 3
let width_of_files _term = files_width
let width_of_preview term = fst (Term.size term) - width_of_files term - 1

let init_state turbo xlang xtargets term =
  {
    xlang;
    xtargets;
    file_zipper =
      Lock_protected.protect
        (ref (Framed_zipper.empty_with_max_len (height_of_preview term)));
    should_continue_iterating_targets = ref true;
    cur_line_rev = [];
    formula = None;
    menu_mode = Navigator;
    pattern_mode = true;
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
    file_zipper =
      Lock_protected.with_lock
        (fun file_zipper -> Lock_protected.protect (ref !file_zipper))
        old_state.file_zipper;
    should_continue_iterating_targets = ref true;
  }

let get_current_line state =
  Common2.string_of_chars (List.rev state.cur_line_rev)

let safe_subtract x y =
  let res = x - y in
  if res < 0 then 0 else res

let parse_event = function
  | `End -> End
  | `Key k -> Key k
  | `Mouse m -> Mouse m
  | `Paste p -> Paste p
  | `Resize (cols, rows) -> Resize { cols; rows }

let bg_if b attr = if b then attr else A.empty

(*****************************************************************************)
(* Pattern building helpers *)
(*****************************************************************************)

(* The base logic for moving up and down through the pattern builder. This
   entails moving through a zipper of zippers, so we have to ensure that we
   only move in an outer zipper if it is not possible to move in the inner
   zipper. In other words, by doing a depth-first search, the first move
   should be the only one.
*)
let move_pat_base ~at_end ~move_zipper state =
  let has_moved = ref false in
  let rec move_iformula = function
    | IPat (xpat, b) -> IPat (xpat, b)
    | Header -> Header
    | IAll zipper ->
        let res = Zipper.map_current move_iformula zipper in
        if !has_moved || at_end zipper then IAll res
        else (
          has_moved := true;
          IAll (move_zipper res))
    | IAny zipper ->
        let res = Zipper.map_current move_iformula zipper in
        if !has_moved || at_end zipper then IAny res
        else (
          has_moved := true;
          IAny (move_zipper res))
  in
  match state.formula with
  | None -> None
  | Some iformula_zipper -> Some (move_iformula iformula_zipper)

let move_pat_up state =
  move_pat_base ~move_zipper:Zipper.move_up ~at_end:Zipper.is_top state

let move_pat_down state =
  move_pat_base ~move_zipper:Zipper.move_down ~at_end:Zipper.is_bottom state

(* Map the xpat which is currently being focused by our pattern builder zipper. *)
let map_focused_pat_string f state =
  let rec loop pat =
    match pat with
    | IPat (xpat, b) ->
        let new_xpat = f xpat in
        IPat (new_xpat, b)
    | Header -> Header
    | IAll pats -> IAll (Zipper.map_current loop pats)
    | IAny pats -> IAny (Zipper.map_current loop pats)
  in
  match state.formula with
  | None -> failwith "precondition"
  | Some formula -> loop formula

(* Sometimes we want to load the string of whatever the currently focused
   pattern in the pattern builder menu is, into our REPL cur line buffer.
   This allows us to immediately start editing it as soon as we switch into
   Pattern Builder.
*)
let load_focused_pat_string state =
  let xpat_ref = ref None in
  map_focused_pat_string
    (fun xpat ->
      xpat_ref := Some xpat;
      xpat)
    state
  |> ignore;
  match !xpat_ref with
  | None -> { state with cur_line_rev = [] }
  | Some { Xpattern.pstr = s, _; _ } ->
      { state with cur_line_rev = List.rev (Common2.list_of_string s) }

let is_pattern_menu state =
  match state.menu_mode with
  | Pattern -> true
  | __else__ -> false

(* We use this to get around our headers, because we don't want to map them
   when rendering patterns, because `all:` and `any:` are meant to be
   unindented.
*)
let map_nonfirst f l =
  match l with
  | [] -> []
  | x :: xs -> x :: Common.map f xs

(* Switch menus, when hitting the TAB key! *)
let switch_menu = function
  | Pattern -> Navigator
  | Navigator -> Pattern

(*****************************************************************************)
(* Engine Helpers *)
(*****************************************************************************)

let fk = Tok.unsafe_fake_tok ""

let translate_formula iformula =
  let rec aux = function
    | IPat (pat, true) -> Some (Rule.P pat)
    | IPat (pat, false) -> Some (Rule.Not (fk, P pat))
    | Header -> None
    | IAll ipats ->
        let pats =
          Zipper.to_list ipats |> Common.map fst |> Common.map_filter aux
        in
        Some (Rule.And (fk, { conjuncts = pats; conditions = []; focus = [] }))
    | IAny ipats ->
        let pats =
          Zipper.to_list ipats |> Common.map fst |> Common.map_filter aux
        in
        Some (Rule.Or (fk, pats))
  in
  match aux iformula with
  | None -> failwith "should not happen"
  | Some iformula -> iformula

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

let atomic_map_file_zipper f state =
  Lock_protected.with_lock
    (fun file_zipper -> file_zipper := f !file_zipper)
    state.file_zipper

let _reset_file_zipper state =
  atomic_map_file_zipper
    (fun _ -> Framed_zipper.empty_with_max_len (height_of_preview state.term))
    state

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

let buffer_matches_of_xtarget (fake_rule : Rule.search_rule) xconf xtarget =
  let hook _s (_m : Pattern_match.t) = () in
  let ({ Report.matches; _ } : _ Report.match_result) =
    (* Calling the engine! *)
    Match_search_mode.check_rule fake_rule hook xconf xtarget
  in
  let _ = matches in
  ()

(* [buffer_matches_of_new_iformula] is an intensive call, which causes the
   state.file_zipper to be imperatively updated every time that we finish
   computing matches on a given file.
   It's called "buffer", in the same way that YouTube videos will buffer their
   contents so that you can replay it faster. We display the file results
   quickly up front so we don't have to wait for the whole time.
*)
let buffer_matches_of_new_iformula (new_iform : iformula_zipper)
    (xlang, xtargets) : unit =
  let rule_formula = translate_formula new_iform in
  let fake_rule = mk_fake_rule (Rule.languages_of_xlang xlang) rule_formula in
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
  xtargets
  |> List.iter (fun xtarget_prot ->
         xtarget_prot
         |> Lock_protected.with_lock (fun xtarget ->
                buffer_matches_of_xtarget fake_rule xconf xtarget))
  |> ignore

let parse_pattern_opt s (xlang, xtargets) =
  try
    let lang = Xlang.to_lang_exn xlang in
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
    let l_rev, m_rev, r_rev, _ = ([], [], [], 0) in
    ( Common2.string_of_chars (List.rev l_rev),
      Common2.string_of_chars (List.rev m_rev),
      Common2.string_of_chars (List.rev r_rev) )

let preview_of_match { Pattern_match.range_loc = t1, t2; _ } file state =
  let lines = Common2.cat file in
  let start_line = t1.pos.line in
  let end_line = t2.pos.line in
  let max_height = height_of_preview state.term in
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
      |> List.fold_left max 0
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
    |> I.vsnap (height_of_preview state.term))

let no_matches_found_img state =
  let h = height_of_preview state.term in
  I.(
    (get_ghost_lines h |> Common.map (I.string (A.fg light_blue)))
    @ [ vpad 2 0 (string A.empty "no matches found") ]
    |> Common.map (hsnap (width_of_preview state.term))
    |> vcat |> I.vsnap h)

let render_preview_no_matches ~has_changed_query state =
  if state.turbo then
    (* In Turbo Mode, what we display here is dependent on two
        things, the current state of the buffer and whether we
        actually changed the query bar with our last key press.
    *)
    if String.equal (get_current_line state) "" then
      default_screen_img "(type a pattern to get started!)" state
    else if has_changed_query then default_screen_img "thinking..." state
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
let render_patterns iformula state =
  let rec loop ~header (pat, is_focus) =
    (* To render a pattern, we have to be able to render the sub-patterns,
       and then put them into place via indenting and adding hyphens.
       To make sure that we highlight the focused pattern properly,
       we carry down an "is_focus" accumulator, which should only be true
       if the path we have taken to this pattern has only been along the
       focused parts of the zipper.
    *)
    let pat_bg =
      bg_if (is_focus && is_pattern_menu state) A.(st bold ++ bg_file_selected)
    in
    match pat with
    | IPat ({ Xpattern.pstr = s, _; _ }, b) ->
        let img =
          if b then I.(string pat_bg (Common.spf "%s" s))
          else I.(string A.(fg lightblue ++ pat_bg) "not: " <|> string pat_bg s)
        in
        img
    | Header ->
        if header then I.(string A.(fg lightblue ++ pat_bg) "all:")
        else I.(string A.(fg lightblue ++ pat_bg) "any:")
    | IAll pats ->
        I.(
          pats |> Zipper.to_list
          |> Common.map (fun (pat, is_focus') ->
                 loop ~header:true (pat, is_focus && is_focus'))
          |> map_nonfirst (fun img ->
                 I.(hpad 2 0 (string A.empty "- " <|> img)))
          |> vcat)
    | IAny pats ->
        I.(
          pats |> Zipper.to_list
          |> Common.map (fun (pat, is_focus') ->
                 loop ~header:false (pat, is_focus && is_focus'))
          |> map_nonfirst (fun img ->
                 I.(hpad 2 0 (string A.empty "- " <|> img)))
          |> vcat)
  in
  (* It doesn't actually matter what this is. By precondition, we should only hit
     Header (which consumes this) if we first pass through IAll or IAny, which
     resets this.
  *)
  let header = true in
  match iformula with
  | IPat (_, true) ->
      I.(string A.(fg lightblue) "pattern: " <|> loop ~header (iformula, true))
  | _ -> loop ~header (iformula, true)

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
        let intermediary_bar =
          String.make (width_of_files state.term) '-'
          |> I.string (A.fg (A.gray 12))
        in
        I.(
          intermediary_bar
          <-> render_patterns formula state
          |> I.hsnap (width_of_files state.term) ~align:`Left)
  in
  let lines_of_files = height_of_preview state.term - I.height patterns in
  let file_zipper = Framed_zipper.set_frame_size lines_of_files file_zipper in
  atomic_map_file_zipper (fun _ -> file_zipper) state;
  let files =
    file_zipper
    |> Framed_zipper.take lines_of_files
    |> Common.mapi (fun idx { file; _ } ->
           if idx = Framed_zipper.relative_position file_zipper then
             I.string
               A.(
                 fg (gray 19)
                 ++ bg_if
                      (not (is_pattern_menu state))
                      (st bold ++ bg_file_selected))
               file
           else I.string (A.fg (A.gray 16)) file)
  in
  I.(
    files |> I.vcat
    |> I.vpad 0 (lines_to_pad_below_to_reach files lines_of_files)
    <-> patterns)

(*****************************************************************************)
(* User Interface (Screen) *)
(*****************************************************************************)

let render_screen ?(has_changed_query = false) state =
  let w, _h = Term.size state.term in
  (* Minus two, because one for the line, and one for
     the input line.
  *)
  (* The zipper might be changing during this call, because in Turbo mode,
     the zipper imperatively changes while we are buffering the matches from
     the turbo thread.
     If we dereference it now and just use its value, we should be good.
  *)
  let file_zipper =
    Lock_protected.with_lock (fun file_zipper -> !file_zipper) state.file_zipper
  in
  let top_left_pane = render_top_left_pane file_zipper state in
  let preview_pane =
    if Framed_zipper.is_empty file_zipper then
      render_preview_no_matches ~has_changed_query state
    else
      let { file; matches = matches_zipper } =
        Framed_zipper.get_current file_zipper
      in
      (* 1 indexed *)
      let match_idx = Zipper.position matches_zipper + 1 in
      let total_matches = Zipper.length matches_zipper in
      let match_position_img =
        if total_matches = 1 then I.void 0 0
        else
          I.string bg_match_position
            (Common.spf "%d/%d" match_idx total_matches)
          |> I.hsnap ~align:`Right (w - files_width - 1)
      in
      let pm = Zipper.get_current matches_zipper in
      I.(match_position_img </> preview_of_match pm file state)
  in
  let vertical_bar = I.char A.empty '|' 1 (height_of_preview state.term) in
  let horizontal_bar =
    let files_loaded_img =
      I.
        [
          string (A.fg semgrep_green) "[";
          string
            (A.fg (A.gray 17))
            (Common.spf "%d/%d"
               (Framed_zipper.length file_zipper)
               (List.length state.xtargets));
          string (A.fg semgrep_green) "]";
        ]
      |> I.hcat |> I.hpad 4 0 |> I.hsnap ~align:`Left w
    in
    I.(files_loaded_img </> (String.make w '-' |> I.string (A.fg (A.gray 12))))
  in
  let mode =
    if state.turbo then I.void 0 0
    else if is_pattern_menu state then
      I.(string A.(fg light_blue) "[PATTERN BUILDER]")
    else if state.pattern_mode then I.(string A.(fg semgrep_green) "[ALL]")
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
  | "not" ->
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
      match (state.formula, state.pattern_mode) with
      | None, _ -> new_pat
      | Some (IAll pats), true -> IAll (Zipper.append new_pat pats)
      | Some (IAny pats), false -> IAny (Zipper.append new_pat pats)
      (* Here is where Header is introduced, when we create a new any or all. *)
      | Some pat, true -> IAll (Zipper.of_list [ Header; pat; new_pat ])
      | Some pat, false -> IAny (Zipper.of_list [ Header; pat; new_pat ])
  in
  let state =
    match cmd with
    | Exit -> failwith "bye bye"
    | All -> { state with pattern_mode = true }
    | Any -> { state with pattern_mode = false }
    | Pat (pat, b) ->
        let new_iformula = handle_pat (pat, b) in
        (* This is a blocking call, so calling this function will
           just cause the REPL thread to block until all the matches
           are done.
           TODO: We could make use of the buffering here too.
        *)
        buffer_matches_of_new_iformula new_iformula (state.xlang, state.xtargets);
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
  (* Let's only spawn a new turbo thread if our next event isn't
     one which is going to spin up yet another turbo thread.
     This reduces perceived lag if the user is typing really fast.
  *)
  (* remove this thread.create call and no SIGBUS is produced *)
  Thread.create
    (fun _ ->
      let pat_opt = parse_pattern_opt "1" state in
      match pat_opt with
      | None ->
          (* When we go back to the empty line, or find no matches,
              reset the view to the no matches screen. *)
          should_refresh := true
      | Some pat ->
          let new_iformula = IPat (pat, true) in
          buffer_matches_of_new_iformula new_iformula state)
    ()
  |> ignore

let stop_thread_if_turbo state =
  if state.turbo then state.should_continue_iterating_targets := false

(*****************************************************************************)
(* Interactive loop *)
(*****************************************************************************)

(* This thread's job is to constantly check for events (and consistently block
   if they do not occur).

   When an event is read, it will be added to a global queue, which will then
   signal to the interactive loop that an event has occurred, and cause a
   rerender.
*)
let spawn_event_thread term =
  Thread.create
    (fun _ ->
      let rec loop () =
        let e = parse_event (Term.event term) in
        Queue.add e event_queue;
        loop ()
      in
      loop ())
    ()
  |> ignore

let interactive_loop ~turbo xlang xtargets =
  (*
  let rec render_and_loop ?(has_changed_query = false) (t : Term.t) state =
    Term.image t (render_screen ~has_changed_query state);
    loop t state
  and on_event_pattern (e : event) state =
    (* We shouldn't be able to enter Pattern Builder mode if we have
       no formula zipper.
    *)
    assert (Option.is_some state.formula);
    let t = state.term in
    match e with
    | Key (`Tab, _) ->
        render_and_loop t
          {
            state with
            menu_mode = switch_menu state.menu_mode;
            cur_line_rev = [];
          }
    | Key (`Arrow `Up, _) ->
        let state = { state with formula = move_pat_up state } in
        let state = load_focused_pat_string state in
        render_and_loop t state
    | Key (`Arrow `Down, _) ->
        let state = { state with formula = move_pat_down state } in
        let state = load_focused_pat_string state in
        render_and_loop t state
    | Key (`Enter, _) -> (
        match parse_pattern_opt (get_current_line state) state with
        | None ->
            (* TODO: better error handling *)
            render_and_loop t
              { state with menu_mode = switch_menu state.menu_mode }
        | Some xpat ->
            let new_formula = map_focused_pat_string (fun _ -> xpat) state in
            (* Load the matches from this new formula! *)
            buffer_matches_of_new_iformula new_formula state;
            render_and_loop t { state with formula = Some new_formula })
    | Key (`Backspace, _) -> (
        match state.cur_line_rev with
        (* nothing to delete, just stay the same *)
        | [] -> loop t state
        | _ :: cs ->
            let state = fresh_state { state with cur_line_rev = cs } in
            render_and_loop ~has_changed_query:true t state)
    | Key (`ASCII c, _) ->
        assert (not state.turbo);
        let state =
          fresh_state { state with cur_line_rev = c :: state.cur_line_rev }
        in
        (* It doesn't actually matter what we put here, because we shouldn't
           be able to get here if we're Turbo, which is the only case this
           matters.
        *)
        render_and_loop ~has_changed_query:true t state
    | _ -> render_and_loop t state
  and on_event_navigator (e : event) state =
    let t = state.term in
    match e with
    | Key (`Tab, _) -> (
        match state.formula with
        | None -> loop t state
        | Some _ ->
            let state = load_focused_pat_string state in
            render_and_loop t
              { state with menu_mode = switch_menu state.menu_mode })
    | Key (`Enter, _) ->
        let state = execute_command state in
        render_and_loop t state
    | Key (`Backspace, _) -> (
        match state.cur_line_rev with
        (* nothing to delete, just stay the same *)
        | [] -> loop t state
        | _ :: cs ->
            stop_thread_if_turbo state;
            let state = fresh_state { state with cur_line_rev = cs } in
            spawn_thread_if_turbo state;
            render_and_loop ~has_changed_query:true t state)
    | Key (`ASCII c, _) ->
        stop_thread_if_turbo state;
        let state =
          fresh_state { state with cur_line_rev = c :: state.cur_line_rev }
        in
        spawn_thread_if_turbo state;
        render_and_loop ~has_changed_query:true t state
    | Key (`Arrow `Left, _) ->
        atomic_map_file_zipper
          (Framed_zipper.map_current (fun { file; matches = mz } ->
               { file; matches = Zipper.move_up mz }))
          state;
        render_and_loop t state
    | Key (`Arrow `Right, _) ->
        atomic_map_file_zipper
          (Framed_zipper.map_current (fun { file; matches = mz } ->
               { file; matches = Zipper.move_down mz }))
          state;
        render_and_loop t state
    | Key (`Arrow `Up, _) ->
        atomic_map_file_zipper Framed_zipper.move_up state;
        render_and_loop t state
    | Key (`Arrow `Down, _) ->
        atomic_map_file_zipper Framed_zipper.move_down state;
        render_and_loop t state
    | Resize _ -> render_and_loop t state
    | __else__ -> render_and_loop t state
  and loop t state =
    if !should_refresh then (
      (* If this is true, this indicates that we should refresh, because a
         thread has asynchronously given us new matches (and a new file zipper)
      *)
      should_refresh := false;
      render_and_loop t state)
    else
      match (Queue.take_opt event_queue, state.menu_mode) with
      | None, _ ->
          (* We have to yield here, or else the turbo thread will take a long time
             to compute, because we're hogging all the process' time!
          *)
          Thread.yield ();
          loop t state
      | Some e, Navigator -> on_event_navigator e state
      | Some e, Pattern -> on_event_pattern e state
  in
  let _ = render_and_loop in
  *)
  (* fake if to shutdown warning 21 of ocamlc "nonreturn-statement" *)
  (* if true then render_and_loop state.term state *)
  spawn_thread_if_turbo (xlang, xtargets);
  Unix.sleep 10;
  ()
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
    targets
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
