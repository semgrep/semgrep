open Common
module Out = Semgrep_output_v1_t
module Sarif = Sarif.Sarif_v_2_1_0_v

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Formats the CLI output to the SARIF format using the sarif OPAM package.
 *
 * Originally written based on:
 *  - https://help.github.com/en/github/finding-security-vulnerabilities-and-errors-in-your-code/about-sarif-support-for-code-scanning
 *   - Which links to this schema:
 *     https://github.com/oasis-tcs/sarif-spec/blob/master/Schemata/sarif-schema-2.1.0.json
 *
 * Full spec:
 *  https://docs.oasis-open.org/sarif/sarif/v2.1.0/sarif-v2.1.0.html (2023)
 *
 * coupling: if you modify which fields are gated by ctx.is_logged_in update also
 * https://semgrep.dev/docs/semgrep-appsec-platform/json-and-sarif#sarif
 *
 * Ported from formatters/sarif.py
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* SARIF v2.1.0-compliant severity string.
 * See the "level" property in the spec
 * See https://github.com/oasis-tcs/sarif-spec/blob/a6473580/Schemata/sarif-schema-2.1.0.json#L1566
 *)
let severity_of_severity sev : Sarif.notification_level =
  match sev with
  | `Info
  | `Low ->
      `Note
  | `Warning
  | `Medium ->
      `Warning
  (* both critical and high are mapped to the same `Error *)
  | `Error
  | `Critical
  | `High ->
      `Error
  | `Experiment
  | `Inventory ->
      raise Todo

let message ?markdown text = Sarif.create_message ?markdown ~text ()

let multiformat_message ?markdown text =
  Sarif.create_multiformat_message_string ?markdown ~text ()

let region ?message ?snippet (start : Out.position) (end_ : Out.position) =
  (* The sarif package is a bit annoying by using int64 for posititons *)
  let start_line = Int64.of_int start.line
  and start_column = Int64.of_int start.col
  and end_line = Int64.of_int end_.line
  and end_column = Int64.of_int end_.col in
  let snippet =
    Option.map (fun text -> Sarif.create_artifact_content ~text ()) snippet
  in
  Sarif.create_region ~start_line ~start_column ~end_line ~end_column ?message
    ?snippet ()

(* Tags to display on SARIF-compliant UIs, such as GitHub security scans. *)
let tags_of_metadata metadata =
  (* XXX: Tags likely have to be strings, but what do we do with non-string json?! *)
  let best_effort_string = function
    | JSON.String s -> s
    | non_string -> JSON.string_of_json non_string
  in
  (* Also add the "security" tag when the rule has CWE tags. *)
  let cwe =
    match JSON.member "cwe" metadata with
    | Some (JSON.Array cwe) -> List_.map best_effort_string cwe @ [ "security" ]
    | Some single_cwe -> [ best_effort_string single_cwe; "security" ]
    | None -> []
  in
  let owasp =
    match JSON.member "owasp" metadata with
    | Some (JSON.Array owasp) ->
        List_.map (fun o -> "OWASP-" ^ best_effort_string o) owasp
    | Some o -> [ "OWASP-" ^ best_effort_string o ]
    | None -> []
  in
  let confidence =
    match JSON.member "confidence" metadata with
    | Some c -> [ best_effort_string c ^ " CONFIDENCE" ]
    | None -> []
  in
  let semgrep_policy_slug =
    match JSON.member "semgrep.policy" metadata with
    | Some (JSON.Object _ as sp) -> (
        match JSON.member "slug" sp with
        | Some slug -> [ best_effort_string slug ]
        | None -> [])
    | Some _
    | None ->
        []
  in
  let tags =
    match JSON.member "tags" metadata with
    | Some (JSON.Array tags) -> List_.map best_effort_string tags
    | Some _
    | None ->
        []
  in
  let all_tags = cwe @ owasp @ confidence @ semgrep_policy_slug @ tags in
  List.sort_uniq String.compare all_tags

(* We want to produce a JSON object with the following shape:
   { id; name;
     defaultConfiguration = { level };
     shortDescription; fullDescription;
     helpUri; help;
     properties
   }
*)
let rule ~(hide_nudge : bool) (ctx : Out.format_context) (rule : Rule.t) :
    Sarif.reporting_descriptor =
  ignore ctx;
  (* in SARIF the definition of the finding,
   * including the severity of the finding is stored within "rules".
   * The results then reference the ID of the rule
   *)
  let rule_id_str = Rule_ID.to_string (fst rule.id) in
  let default_configuration =
    Sarif.create_reporting_configuration
      ~level:(severity_of_severity rule.severity)
      ()
  in
  (* metadata to SARIF official fields *)
  let metadata = rule.Rule.metadata ||| JSON.Null in
  let short_description =
    match JSON.member "shortDescription" metadata with
    | Some (JSON.String shortDescription) -> shortDescription
    | Some _ -> raise Impossible
    | None -> spf "Semgrep Finding: %s" rule_id_str
  in
  let source =
    match JSON.member "source" metadata with
    | Some (JSON.String source) -> Some source
    | Some _
    | None ->
        None
  in
  let rule_help_text =
    match JSON.member "help" metadata with
    | Some (JSON.String txt) -> txt
    | Some _
    | None ->
        rule.message
  in
  let security_severity =
    (* TODO: no test case for this *)
    match JSON.member "security-severity" metadata with
    | Some json ->
        [ ("security-severity", (JSON.to_yojson json :> Yojson.Safe.t)) ]
    | None -> []
  in
  let properties =
    let tags = tags_of_metadata metadata in
    [
      ("precision", `String "very-high");
      ("tags", `List (List_.map (fun s -> `String s) tags));
    ]
    @ security_severity
  in
  (* nudge *)
  let nudge_base = "💎 Enable cross-file analysis and Pro rules for free at"
  and nudge_url = "sg.run/pro" in
  let nudge_plaintext = spf "\n%s %s" nudge_base nudge_url
  and nudge_md =
    spf "\n\n#### %s <a href='https://%s'>%s</a>" nudge_base nudge_url nudge_url
  in
  let text_suffix = if hide_nudge then "" else nudge_plaintext in
  let markdown_interstitial = if hide_nudge then "" else nudge_md in
  let references =
    Option.to_list (Option.map (fun s -> spf "[Semgrep Rule](%s)" s) source)
  in
  let other_references =
    match JSON.member "references" metadata with
    | Some (JSON.String s) -> [ spf "[%s](%s)" s s ]
    | Some (JSON.Array xs) ->
        List_.map
          (function
            | JSON.String s -> spf "[%s](%s)" s s
            | non_string -> JSON.string_of_json non_string)
          xs
    | Some _
    | None ->
        []
  in
  let references_joined =
    List_.map (fun s -> spf " - %s\n" s) (references @ other_references)
  in
  let references_markdown =
    match references_joined with
    | [] -> ""
    | xs -> "\n\n<b>References:</b>\n" ^ String.concat "" xs
  in
  Sarif.create_reporting_descriptor ~id:rule_id_str ~name:rule_id_str
    ~short_description:(multiformat_message short_description)
    ~full_description:(multiformat_message rule.message)
    ~default_configuration
    ~help:
      (multiformat_message
         ~markdown:(rule_help_text ^ markdown_interstitial ^ references_markdown)
         (rule_help_text ^ text_suffix))
    ?help_uri:source ~properties ()

let sarif_fixes (cli_match : Out.cli_match) : Sarif.fix list option =
  let* fixed_lines = cli_match.extra.fixed_lines in
  let description_text =
    spf "%s\n Autofix: Semgrep rule suggested fix" cli_match.extra.message
  in
  let fix =
    let artifact_change =
      Sarif.create_artifact_change
        ~artifact_location:
          (Sarif.create_artifact_location
             ~uri:(Fpath.to_string cli_match.path)
             ())
        ~replacements:
          [
            Sarif.create_replacement
              ~deleted_region:(region cli_match.start cli_match.end_)
              ~inserted_content:
                (Sarif.create_artifact_content
                   ~text:(String.concat "\n" fixed_lines)
                   ())
              ();
          ]
        ()
    in
    Sarif.create_fix ~description:(message description_text)
      ~artifact_changes:[ artifact_change ] ()
  in
  Some [ fix ]

let thread_flow_location (cli_match : Out.cli_match) message
    (location : Out.location) content nesting_level =
  let location =
    Sarif.create_location ~message
      ~physical_location:
        (Sarif.create_physical_location
           ~region:
             (region ~message ~snippet:content location.start location.end_)
           ~artifact_location:
             (Sarif.create_artifact_location
                ~uri:(Fpath.to_string cli_match.path)
                ())
           ())
      ()
  in
  Sarif.create_thread_flow_location
    ~nesting_level:(Int64.of_int nesting_level)
    ~location ()

let intermediate_var_locations cli_match intermediate_vars =
  intermediate_vars
  |> List_.map (fun ({ location; content } : Out.match_intermediate_var) ->
         let propagation_message_text =
           spf "Propagator : '%s' @ '%s:%d'" content
             (Fpath.to_string location.path)
             location.start.line
           |> message
         in
         thread_flow_location cli_match propagation_message_text location
           content 0)

let thread_flows (cli_match : Out.cli_match)
    (dataflow_trace : Out.match_dataflow_trace) (location : Out.location)
    content =
  (* TODO from sarif.py: deal with taint sink *)
  let intermediate_vars = dataflow_trace.intermediate_vars in
  let source_flow_location =
    let source_message_text =
      spf "Source: '%s' @ '%s:%d'" content
        (Fpath.to_string location.path)
        location.start.line
      |> message
    in
    thread_flow_location cli_match source_message_text location content 0
  in
  let intermediate_var_locations =
    match intermediate_vars with
    | None -> []
    | Some intermediate_vars ->
        intermediate_var_locations cli_match intermediate_vars
  in
  let sink_flow_location =
    let sink_message_text =
      spf "Sink: '%s' @ '%s:%d'"
        (String.trim cli_match.extra.lines) (* rule_match.get_lines() ?! *)
        (Fpath.to_string cli_match.path)
        cli_match.start.line
      |> message
    in
    thread_flow_location cli_match sink_message_text
      {
        Out.start = cli_match.start;
        end_ = cli_match.end_;
        path = cli_match.path;
      }
      cli_match.extra.lines 1
  in
  [
    Sarif.create_thread_flow
      ~locations:
        ((source_flow_location :: intermediate_var_locations)
        @ [ sink_flow_location ])
      ();
  ]

let sarif_codeflow (cli_match : Out.cli_match) : Sarif.code_flow list option =
  match cli_match.extra.dataflow_trace with
  | None
  | Some { Out.taint_source = None; _ } ->
      None
  | Some { Out.taint_source = Some (CliCall _); _ } ->
      Logs.err (fun m ->
          m
            "Emitting SARIF output for unsupported dataflow trace (source is a \
             call)");
      None
  | Some
      ({ taint_source = Some (CliLoc (location, content)); _ } as dataflow_trace)
    ->
      (* TODO from sarif.py: handle taint_sink *)
      let code_flow_message =
        spf "Untrusted dataflow from %s:%d to %s:%d"
          (Fpath.to_string location.path)
          location.start.line
          (Fpath.to_string cli_match.path)
          cli_match.start.line
      in
      let thread_flows =
        thread_flows cli_match dataflow_trace location content
      in
      Some
        [
          Sarif.create_code_flow
            ~message:(message code_flow_message)
            ~thread_flows ();
        ]

let result (ctx : Out.format_context) show_dataflow_traces
    (cli_match : Out.cli_match) : Sarif.result =
  let location =
    let physical_location =
      Sarif.create_physical_location
        ~artifact_location:
          (Sarif.create_artifact_location
             ~uri:(Fpath.to_string cli_match.path)
             ~uri_base_id:"%SRCROOT%" ())
        ~region:
          (region ~snippet:cli_match.extra.lines cli_match.start cli_match.end_)
        ()
    in
    Sarif.create_location ~physical_location ()
  in
  let suppressions =
    match cli_match.extra.is_ignored with
    | None
    | Some false ->
        None
    | Some true -> Some [ Sarif.create_suppression ~kind:`InSource () ]
  in
  let fixes = sarif_fixes cli_match in
  let code_flows =
    if show_dataflow_traces then sarif_codeflow cli_match else None
  in
  let properties =
    match Exposure.of_cli_match_opt cli_match with
    | None -> []
    | Some exposure -> [ ("exposure", `String (Exposure.string_of exposure)) ]
  in
  let fingerprints =
    if ctx.is_logged_in then
      [ ("matchBasedId/v1", cli_match.extra.fingerprint) ]
    else [ ("matchBasedId/v1", Gated_data.msg) ]
  in
  Sarif.create_result
    ~rule_id:(Rule_ID.to_string cli_match.check_id)
    ~message:(message cli_match.extra.message)
    ~locations:[ location ] ~fingerprints ~properties ?code_flows ?fixes
    ?suppressions ()

let error_to_sarif_notification (e : Out.cli_error) =
  let level = severity_of_severity e.level in
  let message = message (e.message ||| (e.long_msg ||| (e.short_msg ||| ""))) in
  let descriptor =
    Sarif.create_reporting_descriptor_reference
      ~id:(Error.string_of_error_type e.type_)
      ()
  in
  Sarif.create_notification ~message ~descriptor ~level ()

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let sarif_output (hrules : Rule.hrules) (ctx : Out.format_context)
    (cli_output : Out.cli_output) ~is_pro ~show_dataflow_traces :
    Sarif.sarif_json_schema =
  let hide_nudge = ctx.is_logged_in || is_pro || not ctx.is_using_registry in
  let engine_label = if is_pro then "PRO" else "OSS" in
  let sarif_schema =
    "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json"
  in
  let show_dataflow_traces = ctx.is_logged_in && show_dataflow_traces in
  let run =
    let rules =
      hrules |> Hashtbl.to_seq |> List.of_seq
      (* sorting for snapshot stability *)
      |> List.sort (fun (aid, _) (bid, _) -> Rule_ID.compare aid bid)
      |> List_.map (fun (_ruleid, r) -> rule ~hide_nudge ctx r)
    in
    let tool =
      let driver =
        Sarif.create_tool_component
          ~name:(spf "Semgrep %s" engine_label)
          ~semantic_version:Version.version ~rules ()
      in
      Sarif.create_tool ~driver ()
    in
    let results =
      cli_output.results |> Semgrep_output_utils.sort_cli_matches
      |> List_.map (result ctx show_dataflow_traces)
    in
    let invocation =
      (* TODO no test case(s) for executionNotifications being non-empty *)
      let tool_execution_notifications =
        cli_output.errors
        |> List.sort (fun (a : Out.cli_error) (b : Out.cli_error) ->
               match (a.path, b.path) with
               (* less: could sort more *)
               | Some a1, Some b1 -> Fpath.compare a1 b1
               | _else_ -> Stdlib.compare a b)
        |> List_.map error_to_sarif_notification
      in
      Sarif.create_invocation ~execution_successful:true
        ~tool_execution_notifications ()
    in
    Sarif.create_run ~tool ~results ~invocations:[ invocation ] ()
  in
  Sarif.create_sarif_json_schema ~version:`TwoDotOneDotZero ~schema:sarif_schema
    ~runs:[ run ] ()
