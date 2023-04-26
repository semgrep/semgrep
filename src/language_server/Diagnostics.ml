open Yojson.Safe.Util
open Lsp
open Types
module Conv = Convert_utils
module In = Input_to_core_t

let diagnostic_of location severity message code codeDescription =
  let code = `String code in
  let diagnostic =
    Diagnostic.create
      ~range:(Conv.range_of_location location)
      ~code ~severity ~source:"Semgrep" ~message
  in
  match codeDescription with
  | None -> diagnostic ()
  | Some codeDescription ->
      let codeDescription =
        (* Provided Uri lib has no of_string so this is a little hack :) *)
        CodeDescription.create ~href:(Uri.t_of_yojson (`String codeDescription))
      in

      diagnostic ~codeDescription ()

let diagnostic_of_match ((m, r) : Processed_run.t) =
  let message =
    match m.extra.message with
    | Some message -> message
    | None -> r.message
  in
  let severity = Conv.string_of_severity r.severity in
  let id = m.rule_id in
  let metadata =
    match r.Rule.metadata with
    | None -> `Assoc []
    | Some json -> JSON.to_yojson json
  in
  (* Not sure why I need this, but some OCaml magic symbols make things work *)
  let metadata = (metadata :> Yojson.Safe.t) in
  let source = metadata |> member "source" in
  let source =
    match source with
    | `String s -> Some s
    | __else__ -> None
  in
  let shortlink = metadata |> member "shortlink" in
  let shortlink =
    match shortlink with
    | `String s -> Some s
    | __else__ -> source
  in
  diagnostic_of m.location severity message id shortlink

let diagnostics_of_file (matches : Processed_run.t list) file =
  let matches =
    List.filter
      (fun ((m, _) : Processed_run.t) -> m.location.path = file)
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
