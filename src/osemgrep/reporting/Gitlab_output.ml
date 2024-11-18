open Common
module Out = Semgrep_output_v1_t
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Output findings compatible with GitLab SAST JSON format.

   - Written based on:
     https://github.com/returntocorp/semgrep-action/blob/678eff1a4269ed04b76631771688c8be860ec4e9/src/semgrep_agent/findings.py#L137-L165
   - Docs:
     https://docs.gitlab.com/ee/user/application_security/sast/#reports-json-format
   - Schema:
     https://gitlab.com/gitlab-org/security-products/security-report-schemas/-/blob/master/dist/sast-report-format.json

   Ported from cli/.../formatter/gitlab_sast.py
*)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let to_gitlab_severity = function
  | `Info -> "Info"
  | `Low -> "Low"
  | `Warning
  | `Medium ->
      "Medium"
  | `Error
  | `High ->
      "High"
  | `Critical -> "Critical"
  | `Experiment
  | `Inventory ->
      "Unknown"

type exposure = Reachable | Undetermined | Unreachable

let string_of_exposure = function
  | Reachable -> "reachable"
  | Undetermined -> "undetermined"
  | Unreachable -> "unreachable"

(* python: from rule_match.py exposure_type() *)
let exposure_opt (cli_match : Out.cli_match) : exposure option =
  let* { reachable; _ } = cli_match.extra.sca_info in
  let metadata = JSON.from_yojson cli_match.extra.metadata in
  match JSON.member "sca-kind" metadata with
  | Some (J.String "upgrade-only") -> Some Reachable
  | Some (J.String "legacy") -> Some Undetermined
  (* TODO: stricter: raise error if Some else_json *)
  | _ -> if reachable then Some Reachable else Some Unreachable

let format_cli_match (cli_match : Out.cli_match) : (string * JSON.yojson) list =
  let metadata = JSON.from_yojson cli_match.extra.metadata in
  let source =
    match JSON.member "source" metadata with
    | Some (J.String s) -> s
    | Some _
    | None ->
        "not available"
  in
  let confidence_details, confidence_flags =
    match JSON.member "confidence" metadata with
    | Some (J.String c) ->
        ( [
            ( "confidence",
              `Assoc
                [
                  ("type", `String "text");
                  ("name", `String "confidence");
                  ("value", `String c);
                ] );
          ],
          if String.equal c "LOW" then
            [
              `Assoc
                [
                  ("type", `String "flagged-as-likely-false-positive");
                  ("origin", `String "Semgrep");
                  ( "description",
                    `String "This finding is from a low confidence rule." );
                ];
            ]
          else [] )
    | Some _
    | None ->
        ([], [])
  in
  let exposure_details, exposure_flags =
    match exposure_opt cli_match with
    | None -> ([], [])
    | Some exposure ->
        ( [
            ( "exposure",
              `Assoc
                [
                  ("type", `String "text");
                  ("name", `String "exposure");
                  ("value", `String (string_of_exposure exposure));
                ] );
          ],
          match exposure with
          | Unreachable ->
              [
                `Assoc
                  [
                    ("type", `String "flagged-as-likely-false-positive");
                    ("origin", `String "Semgrep Supply Chain");
                    ( "description",
                      `String
                        "Semgrep found no way to reach this vulnerability \
                         while scanning your code." );
                  ];
              ]
          | Reachable
          | Undetermined ->
              [] )
  in
  let id =
    Semgrep_hashing_functions.syntactic_id cli_match
    |> Uuidm.of_binary_string |> Option.get |> Uuidm.to_string
  in
  let r =
    [
      ("id", `String id);
      ("category", `String "sast");
      (* CVE is a required field from Gitlab schema.
         It also is part of the determination for uniqueness
         of a detected secret
         /regardless/ of differentiated ID. See issue 262648.
         https://gitlab.com/gitlab-org/gitlab/-/issues/262648
         Gitlab themselves mock a CVE for findings that lack a CVE
         Format: path:hash-of-file-path:check_id *)
      ( "cve",
        `String
          (spf "%s:%s:%s"
             (Fpath.to_string cli_match.path)
             Digestif.SHA256.(
               to_hex (digest_string (Fpath.to_string cli_match.path)))
             (Rule_ID.to_string cli_match.check_id)) );
      ("message", `String cli_match.extra.message);
      (* added to the GitLab SAST schema in 16.x, c.f.
         https://gitlab.com/gitlab-org/gitlab/-/issues/339812
         The above "message" field should be unused in 16.x and later! *)
      ("description", `String cli_match.extra.message);
      ("severity", `String (to_gitlab_severity cli_match.extra.severity));
      ( "scanner",
        `Assoc
          [
            ("id", `String "semgrep");
            ("name", `String "Semgrep");
            ("vendor", `Assoc [ ("name", `String "Semgrep") ]);
          ] );
      ( "location",
        `Assoc
          [
            ("file", `String (Fpath.to_string cli_match.path));
            (* Gitlab only uses line identifiers *)
            ("start_line", `Int cli_match.start.line);
            ("end_line", `Int cli_match.end_.line);
          ] );
      ( "identifiers",
        `List
          [
            `Assoc
              [
                ("type", `String "semgrep_type");
                ( "name",
                  `String ("Semgrep - " ^ Rule_ID.to_string cli_match.check_id)
                );
                ("value", `String (Rule_ID.to_string cli_match.check_id));
                ("url", `String source);
              ];
          ] );
      ("flags", `List (confidence_flags @ exposure_flags));
      ("details", `Assoc (confidence_details @ exposure_details));
    ]
  in
  r

let secrets_format_cli_match (cli_match : Out.cli_match) =
  let r = format_cli_match cli_match in
  let more =
    [
      ("category", `String "secret_detection");
      ( "raw_source_code_extract",
        `List [ `String (cli_match.extra.lines ^ "\n") ] );
      ( "commit",
        `Assoc
          [
            ("date", `String "1970-01-01T00:00:00Z");
            (* Even the native Gitleaks based Gitlab secret detection
               only provides a dummy value for now on relevant hash. *)
            ("sha", `String "0000000");
          ] );
    ]
  in
  r @ more

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

let output f (matches : Out.cli_match list) : JSON.yojson =
  let header =
    [
      ( "$schema",
        `String
          "https://gitlab.com/gitlab-org/security-products/security-report-schemas/-/blob/master/dist/sast-report-format.json"
      );
      ("version", `String "15.0.4");
    ]
  in
  let tool =
    `Assoc
      [
        ("id", `String "semgrep");
        ("name", `String "Semgrep");
        ("url", `String "https://semgrep.dev");
        ("version", `String Version.version);
        ("vendor", `Assoc [ ("name", `String "Semgrep") ]);
      ]
  in
  let start_time = Metrics_.g.payload.started_at
  and end_time = Timedesc.Timestamp.now () in
  let scan =
    `Assoc
      [
        ("start_time", `String (Timedesc.Timestamp.to_rfc3339 start_time));
        ("end_time", `String (Timedesc.Timestamp.to_rfc3339 end_time));
        ("analyzer", tool);
        ("scanner", tool);
        ("version", `String Version.version);
        ("status", `String "success");
        ("type", `String "sast");
      ]
  in
  let vulnerabilities =
    List_.filter_map
      (fun (cli_match : Out.cli_match) ->
        match cli_match.extra.severity with
        | `Experiment
        | `Inventory ->
            None
        | `Critical
        | `High
        | `Medium
        | `Low
        | `Info
        | `Warning
        | `Error ->
            Some (`Assoc (f cli_match)))
      matches
  in
  `Assoc
    (header @ [ ("scan", scan); ("vulnerabilities", `List vulnerabilities) ])

let sast_output (matches : Out.cli_match list) : JSON.yojson =
  output format_cli_match matches

let secrets_output (matches : Out.cli_match list) : JSON.yojson =
  output secrets_format_cli_match matches
