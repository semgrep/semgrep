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
      @ suppression @ fix)
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

let sarif_output hrules (cli_output : OutT.cli_output) =
  let sarif_schema =
    "https://docs.oasis-open.org/sarif/sarif/v2.1.0/os/schemas/sarif-schema-2.1.0.json"
  in
  let engine_label =
    match cli_output.OutT.engine_requested with
    | Some `OSS
    | None ->
        "OSS"
    | Some `PRO -> "PRO"
  in
  let run =
    let hide_nudge =
      (* TODO is_logged_in or is_pro or not is_using_registry *)
      true
    in
    let rules = rules hide_nudge hrules in
    let tool =
      `Assoc
        [
          ( "driver",
            `Assoc
              [
                ("name", `String (spf "Semgrep %s" engine_label));
                ("semanticVersion", `String (*"%%VERSION%%"*) "1.56.0");
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
