module Out = Semgrep_output_v1_t

let ellipsis_string = " ... "
let base_indent = String.make 8 ' '
let findings_indent_depth = String.make 10 ' '

let group_titles = function
  | `Unreachable -> "Unreachable Supply Chain Finding"
  | `Undetermined -> "Undetermined Supply Chain Finding"
  | `Reachable -> "Reachable Supply Chain Finding"
  | `Nonblocking -> "Non-blocking Code Finding"
  | `Blocking -> "Blocking Code Finding"
  | `Merged -> "Code Finding"

let is_blocking (json : Yojson.Basic.t) =
  match Yojson.Basic.Util.member "dev.semgrep.actions" json with
  | `List stuff ->
      List.exists
        (function
          | `String s -> String.equal s "block"
          | _else -> false)
        stuff
  | _else -> false

let pp_finding ~max_chars_per_line ~max_lines_per_finding ~color_output
    ~show_separator ppf (m : Out.cli_match) =
  (* TODO dedent lines *)
  (* TODO coloring, bold *)
  ignore color_output;
  let lines =
    Option.value
      ~default:(String.split_on_char '\n' m.extra.lines)
      m.extra.fixed_lines
  in
  let lines, trimmed =
    let ll = List.length lines in
    let max_lines =
      if max_lines_per_finding = 0 then ll else max_lines_per_finding
    in
    let keep = min ll max_lines in
    if keep = ll then (lines, None)
    else (Common.take keep lines, Some (ll - keep))
  in
  let start_line = m.start.line in
  let stripped, _ =
    List.fold_left
      (fun (stripped, line_number) line ->
        let line, stripped =
          let ll = String.length line in
          if max_chars_per_line > 0 && ll > max_chars_per_line then
            if start_line = line_number && m.start.col > 1 then
              let start, ell_at_end =
                let start_col = m.start.col - 1 in
                if start_col >= ll - max_chars_per_line then
                  (ll - max_chars_per_line, "")
                else (start_col, ellipsis_string)
              in
              ( ellipsis_string
                ^ String.sub line start max_chars_per_line
                ^ ell_at_end,
                true )
            else
              (Str.first_chars line max_chars_per_line ^ ellipsis_string, true)
          else (line, stripped)
        in
        let line_number_str = string_of_int line_number in
        let pad = String.make (11 - String.length line_number_str) ' ' in
        Fmt.pf ppf "%s%s┆ %s@." pad line_number_str line;
        (stripped, succ line_number))
      (false, start_line) lines
  in
  if stripped then
    Fmt.pf ppf
      "%s[shortened a long line from output, adjust with \
       --max-chars-per-line]@."
      findings_indent_depth;
  match trimmed with
  | Some num ->
      Fmt.pf ppf
        "%s [hid %d additional lines, adjust with --max-lines-per-finding]@."
        findings_indent_depth num
  | None ->
      if show_separator then
        Fmt.pf ppf "%s⋮┆%s" findings_indent_depth (String.make 40 '-')

let pp_text_outputs ~max_chars_per_line ~max_lines_per_finding ~color_output ppf
    (matches : Out.cli_match list) =
  (* TODO sorting, skipping path if same as previous *)
  let print_one (last : Out.cli_match option) (cur : Out.cli_match)
      (next : Out.cli_match option) =
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
      if print then Fmt.pf ppf "  %s@." cur.path;
      msg
    in
    let print =
      match last_message with
      | None -> true
      | Some m -> m <> cur.extra.message
    in
    if print then (
      let shortlink =
        match Yojson.Basic.Util.member "shortlink" cur.extra.metadata with
        | `String s -> base_indent ^ "Details: " ^ s
        | _else -> ""
      in
      Fmt.pf ppf "    %s@." cur.check_id;
      (* TODO message wrapping *)
      Fmt.pf ppf "      %s@.%s@.@." cur.extra.message shortlink);
    (* TODO autofix *)
    let same_file =
      match next with
      | None -> false
      | Some m -> m.path = cur.path
    in
    pp_finding ~max_chars_per_line ~max_lines_per_finding ~color_output
      ~show_separator:same_file ppf cur;
    Fmt.pf ppf "@."
  in
  let last, cur =
    List.fold_left
      (fun (last, cur) (next : Out.cli_match) ->
        (match cur with
        | None -> ()
        | Some m -> print_one last m (Some next));
        (cur, Some next))
      (None, None) matches
  in
  match cur with
  | Some m -> print_one last m None
  | None -> ()

let pp_cli_output ~max_chars_per_line ~max_lines_per_finding ~color_output ppf
    (cli_output : Out.cli_output) =
  let groups =
    Common.group_by
      (fun (m : Out.cli_match) ->
        (* TODO: python (text.py):
           if match.product == RuleProduct.sast:
             subgroup = "blocking" if match.is_blocking else "nonblocking"
           else:
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
               return "reachable" if self.extra["sca_info"].reachable else "unreachable"
        *)
        if is_blocking m.Out.extra.Out.metadata then `Blocking else `Nonblocking)
      (List.sort
         (fun (m1 : Out.cli_match) (m2 : Out.cli_match) ->
           match String.compare m2.path m1.path with
           | 0 -> (
               match String.compare m2.check_id m1.check_id with
               | 0 -> (
                   match Int.compare m2.start.line m1.start.line with
                   | 0 -> Int.compare m2.start.col m1.start.col
                   | x -> x)
               | x -> x)
           | x -> x)
         cli_output.results)
  in
  (* if not is_ci_invocation *)
  let groups =
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
         groups
  in
  List.iter
    (fun (group, matches) ->
      (match matches with
      | [] -> ()
      | _non_empty ->
          Fmt_helpers.pp_heading ppf
            (string_of_int (List.length matches) ^ " " ^ group_titles group));
      pp_text_outputs ~max_chars_per_line ~max_lines_per_finding ~color_output
        ppf matches)
    groups
