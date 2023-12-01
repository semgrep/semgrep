open Yojson.Safe.Util
open Lsp
open Lsp.Types
open Jsonrpc
open File.Operators
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

let meth = "semgrep/search"

(** on a semgrep/search request, get the pattern and (optional) language params.
    We then try and parse the pattern in every language (or specified lang), and
    scan like normal, only returning the match ranges per file *)
let on_request runner params =
  match params with
  | None -> None
  | Some params ->
      let params = Structured.yojson_of_t params in
      let pattern = params |> member "pattern" |> to_string in
      let lang_opt = params |> member "language" |> to_string_option in
      let xlang_opt =
        Option.bind lang_opt (fun l -> Some (Xlang.of_string l))
      in
      (* TODO: figure out why rules_from_rules_source_async hangs *)
      (* let src = Rules_source.(Pattern (pattern, xlang_opt, None)) in *)
      let rules_and_origins =
        Rule_fetching.rules_from_pattern (pattern, xlang_opt, None)
      in
      let rules, _ =
        Rule_fetching.partition_rules_and_errors rules_and_origins
      in
      let matches = runner rules in
      let matches_by_file =
        Assoc.group_by (fun (m : OutJ.cli_match) -> !!(m.path)) matches
      in
      let json =
        List_.map
          (fun (file, matches) ->
            let uri = file |> Uri.of_path |> Uri.to_string in
            let ranges =
              matches
              |> List_.map Conv.range_of_cli_match
              |> List_.map Range.yojson_of_t
            in
            `Assoc [ ("uri", `String uri); ("ranges", `List ranges) ])
          matches_by_file
      in
      Some (`Assoc [ ("locations", `List json) ])
