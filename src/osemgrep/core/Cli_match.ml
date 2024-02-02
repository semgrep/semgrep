module OutT = Semgrep_output_v1_t
open Common

let name (c : OutT.cli_match) =
  let transient =
    match JSON.member "semgrep.dev" (JSON.from_yojson c.extra.metadata) with
    | Some dev -> (
        match JSON.member "src" dev with
        | Some (JSON.String x) -> String.equal x "previous_scan"
        | Some _
        | None ->
            false)
    | None -> false
  in
  let default = Rule_ID.to_string c.check_id in
  if transient then
    match JSON.member "semgrep.dev" (JSON.from_yojson c.extra.metadata) with
    | Some dev -> (
        match JSON.member "rule" dev with
        | Some rule -> (
            match JSON.member "rule_name" rule with
            | Some (JSON.String rule) -> rule
            | Some _
            | None ->
                default)
        | None -> default)
    | None -> default
  else Rule_ID.to_string c.check_id

let cli_unique_key (c : OutT.cli_match) =
  (* type-wise this is a tuple of string * string * int * int * string * string option *)
  (* # NOTE: We include the previous scan's rules in the config for
     # consistent fixed status work. For unique hashing/grouping,
     # previous and current scan rules must have distinct check IDs.
     # Hence, previous scan rules are annotated with a unique check ID,
     # while the original ID is kept in metadata. As check_id is used
     # for cli_unique_key, this patch fetches the check ID from metadata
     # for previous scan findings.
     # TODO: Once the fixed status work is stable, all findings should
     # fetch the check ID from metadata. This fallback prevents breaking
     # current scan results if an issue arises.
     self.annotated_rule_name if self.from_transient_scan else self.rule_id,
     str(self.path),
     self.start.offset,
     self.end.offset,
     self.message,
     # TODO: Bring this back.
     # This is necessary so we don't deduplicate taint findings which
     # have different sources.
     #
     # self.match.extra.dataflow_trace.to_json_string
     # if self.match.extra.dataflow_trace
     # else None,
     None,
     # NOTE: previously, we considered self.match.extra.validation_state
     # here, but since in some cases (e.g., with `anywhere`) we generate
     # many matches in certain cases, we want to consider secrets
     # matches unique under the above set of things, but with a priority
     # associated with the validation state; i.e., a match with a
     # confirmed valid state should replace all matches equal under the
     # above key. We can't do that just by not considering validation
     # state since we would pick one arbitrarily, and if we added it
     # below then we would report _both_ valid and invalid (but we only
     # want to report valid, if a valid one is present and unique per
     # above fields). See also `should_report_instead`.
  *)
  ( name c,
    Fpath.to_string c.path,
    c.start.offset,
    c.end_.offset,
    c.extra.message,
    None )

let ci_unique_key ?(index = 0) (c : OutT.cli_match) =
  (* TODO the third element should be "syntactic_context", as defined in rule_match.py:
        # The code that matched, with whitespace and nosem comments removed.
        #
        # This is useful to so that findings can be considered the same
        # when `    5 == 5` is updated to `  5 == 5  # nosemgrep`,
        # and thus CI systems don't retrigger notifications.
        lines = [*self.lines]
        if len(lines) > 0:
            lines[0] = NOSEM_INLINE_COMMENT_RE.sub("", lines[0])
            lines[0] = lines[0].rstrip() + "\n"

        code = "".join(lines)  # the lines end with newlines already
        code = textwrap.dedent(code)
        code = code.strip()
        return code
  *)
  (* TODO the return value in python's ci_unique_key is the hex output of the murmur3 hash,
     here the binary value is returned directly *)
  let repr = Python_str_repr.repr in
  spf "(%s, %s, %s, %u)"
    (repr (name c))
    (repr (Fpath.to_string c.path))
    (repr (String.trim c.extra.lines))
    index
  |> Murmur3.hash128
