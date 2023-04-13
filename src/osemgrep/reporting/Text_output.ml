module Out = Semgrep_output_v1_t

let ellipsis_string = " ... "

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

let pp_finding ~max_chars_per_line ~max_lines_per_finding ~color_output ppf
    (m : Out.cli_match) =
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
                if m.start.col >= ll - max_chars_per_line then
                  (ll - max_chars_per_line, "")
                else (m.start.col, ellipsis_string)
              in
              ( ellipsis_string
                ^ String.sub line start max_chars_per_line
                ^ ell_at_end,
                true )
            else
              (Str.first_chars line max_chars_per_line ^ ellipsis_string, true)
          else (line, stripped)
        in
        Fmt.pf ppf "      %u| %s@." line_number line;
        (stripped, succ line_number))
      (false, start_line) lines
  in
  if stripped then
    Fmt.pf ppf
      "      [shortened a long line from output, adjust with \
       --max-chars-per-line]@.";
  Option.iter
    (fun num ->
      Fmt.pf ppf
        "       [hid %d additional lines, adjust with \
         --max-lines-per-finding]@."
        num)
    trimmed

let pp_text_outputs ~max_chars_per_line ~max_lines_per_finding ~color_output ppf
    (matches : Out.cli_match list) =
  (* TODO sorting, skipping path if same as previous *)
  List.iter
    (fun (m : Out.cli_match) ->
      let shortlink =
        match Yojson.Basic.Util.member "shortlink" m.extra.metadata with
        | `String s -> "      Details: " ^ s
        | _else -> ""
      in
      Fmt.pf ppf "  %s@." m.path;
      Fmt.pf ppf "    %s@." m.check_id;
      (* TODO message wrapping *)
      Fmt.pf ppf "      %s@.%s@.@." m.extra.message shortlink;
      pp_finding ~max_chars_per_line ~max_lines_per_finding ~color_output ppf m;
      Fmt.pf ppf "@.")
    matches

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
      cli_output.results
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
