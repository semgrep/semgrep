open Lsp
open Types
open Lsp_util

(*let logger = Logging.get_logger [ __MODULE__ ]*)

let diagnostic_of location severity message code codeDescription =
  let code = `String code in
  (* Provided Uri lib has no of_string so this is a little hack :) *)
  let diagnostic =
    Diagnostic.create
      ~range:(range_of_location location)
      ~code ~severity ~source:"Semgrep" ~message
  in
  match codeDescription with
  | None -> diagnostic ()
  | Some codeDescription ->
      let codeDescription =
        CodeDescription.create ~href:(Uri.t_of_yojson (`String codeDescription))
      in

      diagnostic ~codeDescription ()

let diagnostic_of_match ((m, r) : Semgrep_output_v1_t.core_match * Rule.rule) =
  let message =
    match m.extra.message with
    | Some message -> message
    | None -> r.message
  in
  let severity = string_of_severity r.severity in
  let id = m.rule_id in
  let metadata =
    match r.Rule.metadata with
    | None -> `Assoc []
    | Some json -> JSON.to_yojson json
  in
  let metadata = (metadata :> Yojson.Safe.t) in
  let source = metadata |> Yojson.Safe.Util.member "source" in
  let source =
    match source with
    | `String s -> Some s
    | _ -> None
  in
  let shortlink = metadata |> Yojson.Safe.Util.member "shortlink" in
  let shortlink =
    match shortlink with
    | `String s -> Some s
    | _ -> source
  in
  diagnostic_of m.location severity message id shortlink

let diagnostics_of_file
    (matches : (Semgrep_output_v1_t.core_match * Rule.rule) list) file =
  let matches =
    Common2.filter
      (fun ((m, _) : Semgrep_output_v1_t.core_match * Rule.rule) ->
        m.location.path = file)
      matches
  in
  let diagnostics = Common.map diagnostic_of_match matches in
  let ranges_overlap (a : Diagnostic.t) (b : Diagnostic.t) =
    if a.range.start.line = b.range.start.line then
      a.range.start.character <= b.range.start.character
    else
      a.range.start.line <= b.range.start.line
      && a.range.end_.line >= b.range.end_.line
  in
  let diagnostics =
    Common.uniq_by
      (fun (a : Diagnostic.t) (b : Diagnostic.t) ->
        a.code = b.code && ranges_overlap a b)
      diagnostics
  in
  let params =
    PublishDiagnosticsParams.create ~uri:(Uri.of_path file) ~diagnostics ()
  in
  Server_notification.PublishDiagnostics params

let diagnostics_of_results results files =
  Common.map (diagnostics_of_file results) files
