module OutJ = Semgrep_output_v1_t
open Fpath_.Operators

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
  Partially translated from formatters/text.py
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ellipsis_string = " ... "
let rule_leading_indent_size = 3

let rule_indent_size =
  rule_leading_indent_size + 4 (* severity icon and 1 for space *)

let detail_indent_size = 10
let findings_indent_size = 12
let rule_leading_indent = String.make rule_leading_indent_size ' '
let detail_indent = String.make detail_indent_size ' '
let findings_indent = String.make findings_indent_size ' '

let text_width =
  let max_text_width = 120 in
  let w = Option.value ~default:max_text_width (Terminal_size.get_columns ()) in
  min w max_text_width

(* TODO: re-enable dynamic size in a separate PR to avoid too many test changes *)
let fill_count = 40

type report_group =
  [ OutJ.validation_state
  | `Unreachable
  | `Undetermined
  | `Reachable
  | `Nonblocking
  | `Blocking
  | `Merged ]

let group_titles : report_group -> string = function
  | `Unreachable -> "Unreachable Supply Chain Finding"
  | `Undetermined -> "Undetermined Supply Chain Finding"
  | `Reachable -> "Reachable Supply Chain Finding"
  | `Nonblocking -> "Non-blocking Code Finding"
  | `Blocking -> "Blocking Code Finding"
  | `Merged -> "Code Finding"
  | `Confirmed_valid -> "Valid Secrets Finding"
  | `Confirmed_invalid -> "Invalid Secrets Finding"
  | `Validation_error -> "Secrets Validation Error"
  | `No_validator -> "Unvalidated Secrets Finding"

let sort_by_groups als =
  (* This is the order that groups will be desplayed in. *)
  let group_order : report_group -> int = function
    | `Blocking -> 1
    | `Reachable -> 2
    | `Confirmed_valid -> 3
    | `Undetermined -> 4
    | `Validation_error -> 5
    | `No_validator -> 6
    | `Nonblocking -> 7
    | `Unreachable -> 8
    | `Confirmed_invalid -> 9
    | `Merged -> 10
  in
  let compare_group x y = group_order x - group_order y in
  als |> List.stable_sort (Common.on compare_group fst)

let is_blocking (json : Yojson.Basic.t) =
  match Yojson.Basic.Util.member "dev.semgrep.actions" json with
  | `List stuff ->
      stuff
      |> List.exists (function
           | `String s -> String.equal s "block"
           | _else -> false)
  | _else -> false

let ws_prefix s =
  let rec index_rec s lim i acc =
    if i >= lim then List.rev acc
    else
      let c = s.[i] in
      if c = ' ' then index_rec s lim (i + 1) (' ' :: acc)
      else if c = '\t' then index_rec s lim (i + 1) ('\t' :: acc)
      else List.rev acc
  in
  index_rec s (String.length s) 0 []

let dedent_lines (lines : string list) =
  let ws_prefixes =
    List.sort compare
      (List_.map_filter
         (fun line ->
           if String.(length (trim line)) = 0 then None
           else Some (ws_prefix line))
         lines)
  in
  let longest_prefix =
    let hd, tl =
      match (ws_prefixes, List.rev ws_prefixes) with
      | hd :: _, tl :: _ -> (hd, tl)
      | [], _whatever
      | _whatever, [] ->
          ([], [])
    in
    let rec eq a b togo acc =
      if togo = 0 then acc
      else
        match (a, b) with
        | hda :: tla, hdb :: tlb ->
            if hda = hdb then eq tla tlb (togo - 1) (acc + 1) else acc
        | [], _whatever
        | _whatever, [] ->
            acc
    in
    eq hd tl (min (List.length hd) (List.length tl)) 0
  in
  ( List_.map
      (fun line ->
        if String.(length (trim line)) = 0 then line
        else Str.string_after line longest_prefix)
      lines,
    longest_prefix )

let wrap ~indent ~width s =
  Logs.debug (fun m -> m "wrap indent=%d width=%d s=%s" indent width s);
  let pre = String.make indent ' ' in
  let rec go pre s acc =
    let real_width = width - indent in
    (* In some context (e.g., pre-commit in CI), the number of columns of
     * your terminal can be small in which case real_width above can become
     * negative, in which case we should stop, otherwise
     * String.rindex_from() below will raise an Invalid_arg exn.
     *)
    if String.length s <= real_width || real_width <= 0 then
      List.rev ((pre, s) :: acc)
    else
      (* here we know String.length s > real_width > 0 *)
      let cut =
        let prev_ws =
          try String.rindex_from s real_width ' ' with
          | Not_found -> 0
        and prev_dash =
          try 1 + String.rindex_from s real_width '-' with
          | Not_found -> 0
        in
        let m = max prev_ws prev_dash in
        if m = 0 then real_width else m
      in
      let e, s =
        (Str.first_chars s cut, String.(trim (sub s cut (length s - cut))))
      in
      go pre s ((pre, e) :: acc)
  in
  go pre s []

let cut s idx1 idx2 =
  Logs.debug (fun m -> m "cut %d (idx1 %d idx2 %d)" (String.length s) idx1 idx2);
  ( Str.first_chars s idx1,
    String.sub s idx1 (idx2 - idx1),
    Str.string_after s idx2 )

let pp_finding ~max_chars_per_line ~max_lines_per_finding ~color_output
    ~show_separator ppf (m : OutJ.cli_match) =
  ignore color_output;
  let lines =
    Option.value
      ~default:(String.split_on_char '\n' m.extra.lines)
      m.extra.fixed_lines
  in
  let lines, dedented = dedent_lines lines in
  let lines, trimmed =
    let ll = List.length lines in
    let max_lines =
      if max_lines_per_finding = 0 then ll else max_lines_per_finding
    in
    let keep = min ll max_lines in
    if keep = ll then (lines, None)
    else (List_.take keep lines, Some (ll - keep))
  in
  let start_line = m.start.line in
  let stripped, _ =
    lines
    |> List.fold_left
         (fun (stripped, line_number) line ->
           let line, line_off, stripped' =
             let ll = String.length line in
             if max_chars_per_line > 0 && ll > max_chars_per_line then
               if start_line = line_number then
                 let start_col = m.start.col - 1 - dedented in
                 let end_col = min (start_col + max_chars_per_line) ll in
                 let data =
                   String.sub line start_col (end_col - start_col - 1)
                 in
                 ( (if start_col > 0 then ellipsis_string else "")
                   ^ data ^ ellipsis_string,
                   m.start.col - 1,
                   true )
               else
                 ( Str.first_chars line max_chars_per_line ^ ellipsis_string,
                   0,
                   true )
             else (line, 0, false)
           in
           let line_number_str = string_of_int line_number in
           let pad =
             String.make
               (findings_indent_size + 1 - String.length line_number_str)
               ' '
           in
           let col c = max 0 (c - 1 - dedented - line_off) in
           let ellipsis_len p =
             if stripped' && p then String.length ellipsis_string else 0
           in
           let start_color =
             if line_number > start_line then 0
             else col m.start.col + ellipsis_len (line_off > 0)
           in
           let end_color =
             max start_color
               (if line_number >= m.end_.line then
                  min
                    (if m.start.line = m.end_.line then
                       start_color + (m.end_.col - m.start.col)
                     else col m.end_.col - ellipsis_len true)
                    (String.length line - ellipsis_len true)
                else String.length line)
           in
           let a, b, c = cut line start_color end_color in
           (* TODO(secrets): Apply masking to b *)
           Fmt.pf ppf "%s%s┆ %s%a%s@." pad line_number_str a
             Fmt.(styled `Bold string)
             b c;
           (stripped' || stripped, succ line_number))
         (false, start_line)
  in
  if stripped then
    Fmt.pf ppf
      "%s[shortened a long line from output, adjust with \
       --max-chars-per-line]@."
      findings_indent;
  match trimmed with
  | Some num ->
      Fmt.pf ppf
        "%s [hid %d additional lines, adjust with --max-lines-per-finding]@."
        findings_indent num
  | None ->
      if show_separator then
        Fmt.pf ppf "%s⋮┆%s" findings_indent (String.make fill_count '-')

let pp_text_outputs ~max_chars_per_line ~max_lines_per_finding ~color_output ppf
    (matches : OutJ.cli_match list) =
  let print_one (last : OutJ.cli_match option) (cur : OutJ.cli_match)
      (next : OutJ.cli_match option) =
    let last_message =
      let print, msg =
        match last with
        | None ->
            Fmt.pf ppf "@.";
            (true, None)
        | Some m ->
            if m.path = cur.path then (false, Some m.extra.message)
            else (true, None)
      in
      (if print then
         (* python compatibility: the 22m and 24m are "normal color or intensity", and "underline off" *)
         let esc =
           if Fmt.style_renderer ppf = `Ansi_tty then
             Fmt.any "\027[22m\027[24m  "
           else Fmt.any "  "
         in
         Fmt.pf ppf "  %a@."
           Fmt.(styled (`Fg `Cyan) (esc ++ string ++ any " "))
           !!(cur.path));
      msg
    in
    let print =
      cur.check_id <> Constants.rule_id_for_dash_e
      &&
      match last_message with
      | None -> true
      | Some m -> m <> cur.extra.message
    in
    if print then (
      let sev_icon_enablement = false in
      (* Enable in a separate PR post-test updates *)
      let pp_styled_severity =
        if Fmt.style_renderer ppf = `Ansi_tty && sev_icon_enablement then
          function
          | `Error ->
              Fmt.pf ppf "%s%a" rule_leading_indent
                Fmt.(styled (`Fg `Red) string)
                "❯❯❱"
          (* No out-of-the-box support for Orange and we use here Magenta instead :/ *)
          | `Warning ->
              Fmt.pf ppf "%s%a" rule_leading_indent
                Fmt.(styled (`Fg `Magenta) string)
                " ❯❱"
          | `Info ->
              Fmt.pf ppf "%s%a" rule_leading_indent
                Fmt.(styled (`Fg `Green) string)
                "  ❱"
          | _ -> Fmt.pf ppf "%s%s" rule_leading_indent "   "
        else function
          | _ -> Fmt.pf ppf "%s%s" rule_leading_indent "   "
      in
      pp_styled_severity cur.extra.severity;
      let lines =
        wrap ~indent:rule_indent_size ~width:text_width
          (Rule_ID.to_string cur.check_id)
      in
      match lines with
      | [] -> ()
      | (_, l) :: rest ->
          (* Print indented severity with 1 trailing space and then first line *)
          Fmt.pf ppf " %a@." Fmt.(styled `Bold string) l;
          List.iter
            (fun (sp, l) -> Fmt.pf ppf "%s%a@." sp Fmt.(styled `Bold string) l)
            rest;
          List.iter
            (fun (sp, l) -> Fmt.pf ppf "%s%s@." sp l)
            (wrap ~indent:detail_indent_size
               ~width:(text_width - detail_indent_size)
               cur.extra.message);
          (match Yojson.Basic.Util.member "shortlink" cur.extra.metadata with
          | `String s -> Fmt.pf ppf "%sDetails: %s@." detail_indent s
          | _else -> ());
          Fmt.pf ppf "@.");
    (* TODO autofix *)
    let same_file =
      match next with
      | None -> false
      | Some m -> m.path = cur.path
    in
    let same_rule =
      match next with
      | None -> false
      | Some m -> m.check_id = cur.check_id
    in
    pp_finding ~max_chars_per_line ~max_lines_per_finding ~color_output
      ~show_separator:(same_file && same_rule) ppf cur;
    Fmt.pf ppf "@."
  in
  let last, cur =
    matches
    |> List.fold_left
         (fun (last, cur) (next : OutJ.cli_match) ->
           (match cur with
           | None -> ()
           | Some m -> print_one last m (Some next));
           (cur, Some next))
         (None, None)
  in
  match cur with
  | Some m -> print_one last m None
  | None -> ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let pp_cli_output ~max_chars_per_line ~max_lines_per_finding ~color_output ppf
    (cli_output : OutJ.cli_output) =
  cli_output.results |> Semgrep_output_utils.sort_cli_matches
  |> Assoc.group_by (fun (m : OutJ.cli_match) ->
         match Product.of_cli_match m with
         | `SCA ->
             (* TO PORT:
                                       subgroup = match.exposure_type or "undetermined"

                        figuring out the product, python uses (rule.py):
                           RuleProduct.sca
                           if "r2c-internal-project-depends-on" in self._raw
                           else RuleProduct.sast

                        and exposure_type (rule_match.py):
                        if "sca_info" not in self.extra:
                            return None

                        if self.metadata.get("sca-kind") == "upgrade-only":
                            return "reachable"
                        elif self.metadata.get("sca-kind") == "legacy":
                            return "undetermined"
                        else:
                            return "reachable" if self.extra["sca_info"].reachable else "unreachable" *)
             `Undetermined
         | `SAST when is_blocking m.extra.metadata -> `Blocking
         | `SAST -> `Nonblocking
         | `Secrets ->
             (Option.value ~default:`No_validator m.extra.validation_state
               :> report_group))
  |> (fun groups ->
       (* TO PORT:
          if not is_ci_invocation: *)
       let merged =
         (try List.assoc `Nonblocking groups with
         | Not_found -> [])
         @
         try List.assoc `Blocking groups with
         | Not_found -> []
       in
       (`Merged, merged)
       :: List.filter
            (fun (k, _) -> not (k = `Nonblocking || k = `Blocking))
            groups)
  |> sort_by_groups
  |> List.iter (fun (group, matches) ->
         if not (List_.null matches) then
           Fmt_.pp_heading ppf
             (String_.unit_str (List.length matches) (group_titles group));
         pp_text_outputs ~max_chars_per_line ~max_lines_per_finding
           ~color_output ppf matches)
