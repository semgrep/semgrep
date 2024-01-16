module OutT = Semgrep_output_v1_t

let sarif_severity_of_severity = function
  | `Info -> "note"
  | `Warning -> "warning"
  | `Error -> "error"
  | `Experiment
  | `Inventory ->
      raise Common.Todo

(* We want to produce a json object? with the following shape:
   { id; name;
     shortDescription; fullDescription;
     helpUri; help;
     defaultConfiguration = { level };
     properties }
*)
let rules (hrules : Rule.hrules) =
  let rules = Hashtbl.to_seq hrules in
  let rules =
    Seq.map
      (fun (rule_id, rule) ->
        let metadata = Option.value ~default:JSON.Null rule.Rule.metadata in
        let short_description =
          match JSON.member "shortDescription" metadata with
          | Some (JSON.String shortDescription) -> shortDescription
          | Some _ -> raise Common.Impossible
          | None -> Common.spf "Semgrep Finding: %s" (Rule_ID.to_string rule_id)
        and rule_url =
          match JSON.member "source" metadata with
          | Some (JSON.String source) -> [ ("helpUri", `String source) ]
          | Some _
          | None ->
              []
        and rule_help_text =
          match JSON.member "help" metadata with
          | Some (JSON.String txt) -> txt
          | Some _
          | None ->
              rule.message
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
                   ("text", `String rule_help_text);
                   (* missing text_suffix *)
                   ("markdown", `String rule_help_text);
                   (* missing: markdown_interstitial references_markdown *)
                 ] );
             (* TODO properties *)
           ]
          @ rule_url))
      rules
  in
  List.of_seq rules
