open Lsp
open Types
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

let diagnostic_of_match is_intellij (m : OutJ.cli_match) =
  let severity = Conv.convert_severity m.extra.severity in
  let message = m.extra.message in
  let check_id_str = Rule_ID.to_string m.check_id in
  let message =
    if String.equal message "" then
      Printf.sprintf "Semgrep found: %s" check_id_str
    else message
  in
  let code = `String check_id_str in
  let shortlink =
    let metadata = (m.extra.metadata :> Yojson.Safe.t) in
    match metadata with
    | `Assoc _ ->
        metadata
        |> Yojson.Safe.Util.member "shortlink"
        |> Yojson.Safe.Util.to_string_option
    | _ -> None
  in
  let message =
    match shortlink with
    (* IntelliJ doesn't display code descriptions:/ so we must insert them here *)
    | Some s when is_intellij ->
        message
        ^ Printf.sprintf "\nSemgrep(<a href=\"%s\">%s</a>)" s check_id_str
    | _ -> message
  in
  let codeDescription =
    Option.bind shortlink (fun s ->
        Some (CodeDescription.create ~href:(Uri.t_of_yojson (`String s))))
  in
  let diagnostic =
    Diagnostic.create
      ~range:(Conv.range_of_cli_match m)
      ~code ~severity ~source:"Semgrep" ~message
  in
  match codeDescription with
  | None -> diagnostic ()
  | Some codeDescription -> diagnostic ~codeDescription ()

let diagnostics_of_file is_intellij matches file =
  let matches =
    List.filter (fun (m : OutJ.cli_match) -> m.path = file) matches
  in
  let diagnostics = List_.map (diagnostic_of_match is_intellij) matches in
  let diagnostics =
    List_.deduplicate_gen
      ~get_key:(fun (x : Diagnostic.t) ->
        x |> Diagnostic.yojson_of_t |> Yojson.Safe.to_string)
      diagnostics
  in
  let params =
    PublishDiagnosticsParams.create
      ~uri:(Uri.of_path (Fpath.to_string file))
      ~diagnostics ()
  in
  Server_notification.PublishDiagnostics params

let diagnostics_of_results ~is_intellij results files =
  List_.map (diagnostics_of_file is_intellij results) files
