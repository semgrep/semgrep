module Out = Semgrep_output_v1_t

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

let pp_finding ppf (m : Out.cli_match) =
  (* TODO trim lines *)
  (* TODO dedent lines *)
  (* TODO strip lines *)
  (* TODO coloring, bold *)
  let lines =
    Option.value
      ~default:(String.split_on_char '\n' m.extra.lines)
      m.extra.fixed_lines
  in
  let start_line = m.start.line in
  List.iteri
    (fun i line -> Fmt.pf ppf "      %u| %s@." (start_line + i) line)
    lines

let pp_text_outputs ppf (matches : Out.cli_match list) =
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
      pp_finding ppf m;
      Fmt.pf ppf "@.")
    matches

let pp_cli_output ppf (cli_output : Out.cli_output) =
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
      pp_text_outputs ppf matches)
    groups
