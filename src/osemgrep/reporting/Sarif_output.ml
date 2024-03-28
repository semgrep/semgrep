open Common
module OutT = Semgrep_output_v1_t
module Sarif_v = Sarif.Sarif_v_2_1_0_v

let sarif_severity_of_severity : _ -> Sarif_v.notification_level = function
  | `Info -> `Note
  | `Warning -> `Warning
  | `Error -> `Error
  | `Experiment
  | `Inventory ->
      raise Todo

let message ?markdown text = Sarif_v.create_message ?markdown ~text ()

let multiformat_message ?markdown text =
  Sarif_v.create_multiformat_message_string ?markdown ~text ()

let region ?message ?snippet (start : OutT.position) (end_ : OutT.position) =
  (* The sarif package is a bit annoying by using int64 for posititons *)
  let start_line = Int64.of_int start.line
  and start_column = Int64.of_int start.col
  and end_line = Int64.of_int end_.line
  and end_column = Int64.of_int end_.col in
  let snippet =
    Option.map (fun text -> Sarif_v.create_artifact_content ~text ()) snippet
  in
  Sarif_v.create_region ~start_line ~start_column ~end_line ~end_column ?message
    ?snippet ()

let tags_of_metadata metadata =
  (* XXX: Tags likely have to be strings, but what do we do with non-string json?! *)
  let best_effort_string = function
    | JSON.String s -> s
    | non_string -> JSON.string_of_json non_string
  in
  let cwe =
    match JSON.member "cwe" metadata with
    | Some (JSON.Array cwe) -> List_.map best_effort_string cwe
    | Some single_cwe -> [ best_effort_string single_cwe ]
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
  cwe @ owasp @ confidence @ semgrep_policy_slug @ tags

(* We want to produce a json object? with the following shape:
   { id; name;
     shortDescription; fullDescription;
     helpUri; help;
     defaultConfiguration = { level };
     properties }
*)
let rules hide_nudge (hrules : Rule.hrules) =
  let rules = Hashtbl.to_seq hrules in
  let rules =
    Seq.map
      (fun (rule_id, rule) ->
        let metadata = Option.value ~default:JSON.Null rule.Rule.metadata in
        let short_description =
          match JSON.member "shortDescription" metadata with
          | Some (JSON.String shortDescription) -> shortDescription
          | Some _ -> raise Impossible
          | None -> spf "Semgrep Finding: %s" (Rule_ID.to_string rule_id)
        and source =
          match JSON.member "source" metadata with
          | Some (JSON.String source) -> Some source
          | Some _
          | None ->
              None
        and rule_help_text =
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
        let nudge_base =
          "💎 Enable cross-file analysis and Pro rules for free at"
        and nudge_url = "sg.run/pro" in
        let nudge_plaintext = spf "\n%s %s" nudge_base nudge_url
        and nudge_md =
          spf "\n\n#### %s <a href='https://%s'>%s</a>" nudge_base nudge_url
            nudge_url
        in
        let text_suffix = if hide_nudge then "" else nudge_plaintext in
        let markdown_interstitial = if hide_nudge then "" else nudge_md in
        let references =
          Option.to_list
            (Option.map (fun s -> spf "[Semgrep Rule](%s)" s) source)
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
        Sarif_v.create_reporting_descriptor
          ~id:(Rule_ID.to_string rule_id)
          ~name:(Rule_ID.to_string rule_id)
          ~short_description:(multiformat_message short_description)
          ~full_description:(multiformat_message rule.message)
          ~default_configuration:
            (Sarif_v.create_reporting_configuration
               ~level:(sarif_severity_of_severity rule.severity)
               ())
          ~help:
            (multiformat_message
               ~markdown:
                 (rule_help_text ^ markdown_interstitial ^ references_markdown)
               (rule_help_text ^ text_suffix))
          ?help_uri:source ~properties ())
      rules
  in
  List.of_seq rules

let sarif_fixes (cli_match : OutT.cli_match) =
  match cli_match.extra.fixed_lines with
  | None -> None
  | Some fixed_lines ->
      let description_text =
        spf "%s\n Autofix: Semgrep rule suggested fix" cli_match.extra.message
      in
      let fix =
        let artifact_change =
          Sarif_v.create_artifact_change
            ~artifact_location:
              (Sarif_v.create_artifact_location
                 ~uri:(Fpath.to_string cli_match.path)
                 ())
            ~replacements:
              [
                Sarif_v.create_replacement
                  ~deleted_region:(region cli_match.start cli_match.end_)
                  ~inserted_content:
                    (Sarif_v.create_artifact_content
                       ~text:(String.concat "\n" fixed_lines)
                       ())
                  ();
              ]
            ()
        in
        Sarif_v.create_fix ~description:(message description_text)
          ~artifact_changes:[ artifact_change ] ()
      in
      Some [ fix ]

let thread_flow_location (cli_match : OutT.cli_match) message
    (location : OutT.location) content nesting_level =
  let location =
    Sarif_v.create_location ~message
      ~physical_location:
        (Sarif_v.create_physical_location
           ~region:
             (region ~message ~snippet:content location.start location.end_)
           ~artifact_location:
             (Sarif_v.create_artifact_location
                ~uri:(Fpath.to_string cli_match.path)
                ())
           ())
      ()
  in
  Sarif_v.create_thread_flow_location
    ~nesting_level:(Int64.of_int nesting_level)
    ~location ()

let intermediate_var_locations cli_match intermediate_vars =
  intermediate_vars
  |> List_.map (fun ({ location; content } : OutT.match_intermediate_var) ->
         let propagation_message_text =
           spf "Propagator : '%s' @ '%s:%d'" content
             (Fpath.to_string location.path)
             location.start.line
           |> message
         in
         thread_flow_location cli_match propagation_message_text location
           content 0)

let thread_flows (cli_match : OutT.cli_match)
    (dataflow_trace : OutT.match_dataflow_trace) (location : OutT.location)
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
        OutT.start = cli_match.start;
        end_ = cli_match.end_;
        path = cli_match.path;
      }
      cli_match.extra.lines 1
  in
  [
    Sarif_v.create_thread_flow
      ~locations:
        ((source_flow_location :: intermediate_var_locations)
        @ [ sink_flow_location ])
      ();
  ]

let sarif_codeflow (cli_match : OutT.cli_match) =
  match cli_match.extra.dataflow_trace with
  | None
  | Some { OutT.taint_source = None; _ } ->
      None
  | Some { OutT.taint_source = Some (CliCall _); _ } ->
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
          Sarif_v.create_code_flow
            ~message:(message code_flow_message)
            ~thread_flows ();
        ]

let results (cli_output : OutT.cli_output) =
  let result (cli_match : OutT.cli_match) =
    let location =
      let physical_location =
        Sarif_v.create_physical_location
          ~artifact_location:
            (Sarif_v.create_artifact_location
               ~uri:(Fpath.to_string cli_match.path)
               ~uri_base_id:"%SRCROOT%" ())
          ~region:
            (region ~snippet:cli_match.extra.lines cli_match.start
               cli_match.end_)
          ()
      in
      Sarif_v.create_location ~physical_location ()
    in
    let suppressions =
      match cli_match.extra.is_ignored with
      | None
      | Some false ->
          None
      | Some true -> Some [ Sarif_v.create_suppression ~kind:`InSource () ]
    in
    let fixes = sarif_fixes cli_match in
    let code_flows = sarif_codeflow cli_match in
    Sarif_v.create_result
      ~rule_id:(Rule_ID.to_string cli_match.check_id)
      ~message:(message cli_match.extra.message)
      ~locations:[ location ]
      ~fingerprints:[ ("matchBasedId/v1", cli_match.extra.fingerprint) ]
      ~properties:[] ?code_flows ?fixes ?suppressions ()
  in
  List_.map result cli_output.results

let error_to_sarif_notification (e : OutT.cli_error) =
  let level = sarif_severity_of_severity e.level in
  let message =
    Option.value
      ~default:
        (Option.value
           ~default:(Option.value ~default:"" e.short_msg)
           e.long_msg)
      e.message
    |> message
  in
  let descriptor =
    Sarif_v.create_reporting_descriptor_reference
      ~id:(Error.string_of_error_type e.type_)
      ()
  in
  Sarif_v.create_notification ~message ~descriptor ~level ()

let sarif_output hide_nudge _dataflow_traces engine_label hrules
    (cli_output : OutT.cli_output) =
  let sarif_schema =
    "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json"
  in
  let run =
    let rules = rules hide_nudge hrules in
    let tool =
      let driver =
        Sarif_v.create_tool_component
          ~name:(spf "Semgrep %s" engine_label)
          ~semantic_version:Version.version ~rules ()
      in
      Sarif_v.create_tool ~driver ()
    in
    let results = results cli_output in
    let invocation =
      (* TODO no test case(s) for executionNotifications being non-empty *)
      let tool_execution_notifications =
        List_.map error_to_sarif_notification cli_output.errors
      in
      Sarif_v.create_invocation ~execution_successful:true
        ~tool_execution_notifications ()
    in
    Sarif_v.create_run ~tool ~results ~invocations:[ invocation ] ()
  in
  Sarif_v.create_sarif_json_schema ~version:`TwoDotOneDotZero
    ~schema:sarif_schema ~runs:[ run ] ()
