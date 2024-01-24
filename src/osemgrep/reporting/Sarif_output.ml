open Common
module OutT = Semgrep_output_v1_t

let sarif_severity_of_severity = function
  | `Info -> "note"
  | `Warning -> "warning"
  | `Error -> "error"
  | `Experiment
  | `Inventory ->
      raise Todo

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
        and source, rule_url =
          match JSON.member "source" metadata with
          | Some (JSON.String source) ->
              (Some source, [ ("helpUri", `String source) ])
          | Some _
          | None ->
              (None, [])
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
          | Some json -> [ ("security-severity", JSON.to_yojson json) ]
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
        `Assoc
          ([
             ("id", `String (Rule_ID.to_string rule_id));
             ("name", `String (Rule_ID.to_string rule_id));
             ("shortDescription", `Assoc [ ("text", `String short_description) ]);
             ("fullDescription", `Assoc [ ("text", `String rule.message) ]);
             ( "defaultConfiguration",
               `Assoc
                 [
                   ("level", `String (sarif_severity_of_severity rule.severity));
                 ] );
             ( "help",
               `Assoc
                 [
                   ("text", `String (rule_help_text ^ text_suffix));
                   ( "markdown",
                     `String
                       (rule_help_text ^ markdown_interstitial
                      ^ references_markdown) );
                 ] );
             ("properties", `Assoc properties);
           ]
          @ rule_url))
      rules
  in
  List.of_seq rules

let fixed_lines (cli_match : OutT.cli_match) fix =
  let lines = String.split_on_char '\n' cli_match.extra.lines in
  match (lines, List.rev lines) with
  | line :: _, last_line :: _ ->
      let first_line_part = Str.first_chars line (cli_match.start.col - 1)
      and last_line_part =
        Str.string_after last_line (cli_match.end_.col - 1)
      in
      String.split_on_char '\n' (first_line_part ^ fix ^ last_line_part)
  | [], _
  | _, [] ->
      []

let sarif_fix (cli_match : OutT.cli_match) =
  match cli_match.extra.fixed_lines with
  | None -> []
  | Some fixed_lines ->
      let description_text =
        spf "%s\n Autofix: Semgrep rule suggested fix" cli_match.extra.message
      in
      [
        ( "fixes",
          `List
            [
              `Assoc
                [
                  ("description", `Assoc [ ("text", `String description_text) ]);
                  ( "artifactChanges",
                    `List
                      [
                        `Assoc
                          [
                            ( "artifactLocation",
                              `Assoc
                                [
                                  ( "uri",
                                    `String (Fpath.to_string cli_match.path) );
                                ] );
                            ( "replacements",
                              `List
                                [
                                  `Assoc
                                    [
                                      ( "deletedRegion",
                                        `Assoc
                                          [
                                            ( "startLine",
                                              `Int cli_match.start.line );
                                            ( "startColumn",
                                              `Int cli_match.start.col );
                                            ("endLine", `Int cli_match.end_.line);
                                            ( "endColumn",
                                              `Int cli_match.end_.col );
                                          ] );
                                      ( "insertedContent",
                                        `Assoc
                                          [
                                            ( "text",
                                              `String
                                                (String.concat "\n" fixed_lines)
                                            );
                                          ] );
                                    ];
                                ] );
                          ];
                      ] );
                ];
            ] );
      ]

let sarif_location (cli_match : OutT.cli_match) message
    (location : OutT.location) content nesting_level =
  `Assoc
    [
      ( "location",
        `Assoc
          [
            ("message", `Assoc [ ("text", `String message) ]);
            ( "physicalLocation",
              `Assoc
                [
                  ( "artifactLocation",
                    `Assoc [ ("uri", `String (Fpath.to_string cli_match.path)) ]
                  );
                  ( "region",
                    `Assoc
                      [
                        ("startLine", `Int location.start.line);
                        ("startColumn", `Int location.start.col);
                        ("endLine", `Int location.end_.line);
                        ("endColumn", `Int location.end_.col);
                        ("snippet", `Assoc [ ("text", `String content) ]);
                        ("message", `Assoc [ ("text", `String message) ]);
                      ] );
                ] );
          ] );
      ("nestingLevel", `Int nesting_level);
    ]

let intermediate_var_locations cli_match intermediate_vars =
  intermediate_vars
  |> List_.map (fun ({ location; content } : OutT.match_intermediate_var) ->
         let propagation_message_text =
           spf "Propagator : '%s' @ '%s:%d'" content
             (Fpath.to_string location.path)
             location.start.line
         in
         sarif_location cli_match propagation_message_text location content 0)

let thread_flows (cli_match : OutT.cli_match)
    (dataflow_trace : OutT.match_dataflow_trace) (location : OutT.location)
    content =
  (* TODO from sarif.py: deal with taint sink *)
  let intermediate_vars = dataflow_trace.intermediate_vars in
  ignore cli_match;
  ignore dataflow_trace;
  let thread_flow_location =
    let source_message_text =
      spf "Source: '%s' @ '%s:%d'" content
        (Fpath.to_string location.path)
        location.start.line
    in
    sarif_location cli_match source_message_text location content 0
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
    in
    sarif_location cli_match sink_message_text
      {
        OutT.start = cli_match.start;
        end_ = cli_match.end_;
        path = cli_match.path;
      }
      cli_match.extra.lines 1
  in
  [
    ( "threadFlows",
      `List
        [
          `Assoc
            [
              ( "locations",
                `List
                  ([ thread_flow_location ] @ intermediate_var_locations
                 @ [ sink_flow_location ]) );
            ];
        ] );
  ]

let sarif_codeflow (cli_match : OutT.cli_match) =
  match cli_match.extra.dataflow_trace with
  | None
  | Some { OutT.taint_source = None; _ } ->
      []
  | Some { OutT.taint_source = Some (CliCall _); _ } ->
      Logs.err (fun m ->
          m
            "Emitting SARIF output for unsupported dataflow trace (source is a \
             call)");
      []
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
        ignore dataflow_trace;
        thread_flows cli_match dataflow_trace location content
      in
      [
        ( "codeFlows",
          `List
            [
              `Assoc
                ([ ("message", `Assoc [ ("text", `String code_flow_message) ]) ]
                @ thread_flows);
            ] );
      ]

let results (cli_output : OutT.cli_output) =
  let result (cli_match : OutT.cli_match) =
    let location =
      `Assoc
        [
          ( "physicalLocation",
            `Assoc
              [
                ( "artifactLocation",
                  `Assoc
                    [
                      ("uri", `String (Fpath.to_string cli_match.path));
                      ("uriBaseId", `String "%SRCROOT%");
                    ] );
                ( "region",
                  `Assoc
                    [
                      ( "snippet",
                        `Assoc [ ("text", `String cli_match.extra.lines) ] );
                      ("startLine", `Int cli_match.start.line);
                      ("startColumn", `Int cli_match.start.col);
                      ("endLine", `Int cli_match.end_.line);
                      ("endColumn", `Int cli_match.end_.col);
                    ] );
              ] );
        ]
    in
    let suppression =
      match cli_match.extra.is_ignored with
      | None
      | Some false ->
          []
      | Some true ->
          [
            ("suppressions", `List [ `Assoc [ ("kind", `String "inSource") ] ]);
          ]
    in
    let fix = sarif_fix cli_match in
    let code_flows = sarif_codeflow cli_match in
    `Assoc
      ([
         ("ruleId", `String (Rule_ID.to_string cli_match.check_id));
         ("message", `Assoc [ ("text", `String cli_match.extra.message) ]);
         ("locations", `List [ location ]);
         ( "fingerprints",
           `Assoc [ ("matchBasedId/v1", `String cli_match.extra.fingerprint) ]
         );
         ("properties", `Assoc []);
       ]
      @ suppression @ fix @ code_flows)
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
  in
  let descriptor = Error.string_of_error_type e.type_ in
  `Assoc
    [
      ("descriptor", `Assoc [ ("id", `String descriptor) ]);
      ("message", `Assoc [ ("text", `String message) ]);
      ("level", `String level);
    ]

let sarif_output is_logged_in hrules (cli_output : OutT.cli_output) =
  let sarif_schema =
    "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json"
  in
  let engine_label, is_pro =
    match cli_output.OutT.engine_requested with
    | Some `OSS
    | None ->
        ("OSS", false)
    | Some `PRO -> ("PRO", true)
  in
  let run =
    let hide_nudge =
      is_logged_in || is_pro || not Metrics_.g.is_using_registry
    in
    let rules = rules hide_nudge hrules in
    let tool =
      `Assoc
        [
          ( "driver",
            `Assoc
              [
                ("name", `String (spf "Semgrep %s" engine_label));
                ("semanticVersion", `String Version.version);
                ("rules", `List rules);
              ] );
        ]
    in
    let results = results cli_output in
    let invocation =
      (* TODO no test case(s) for executionNotifications being non-empty *)
      let exec_notifs =
        List_.map error_to_sarif_notification cli_output.errors
      in
      `Assoc
        [
          ("executionSuccessful", `Bool true);
          ("toolExecutionNotifications", `List exec_notifs);
        ]
    in
    `Assoc
      [
        ("tool", tool);
        ("results", `List results);
        ("invocations", `List [ invocation ]);
      ]
  in
  `Assoc
    [
      ("version", `String "2.1.0");
      ("$schema", `String sarif_schema);
      ("runs", `List [ run ]);
    ]
