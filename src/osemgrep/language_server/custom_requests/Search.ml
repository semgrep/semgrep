open Lsp
open Lsp.Types
open Fpath_.Operators
module Conv = Convert_utils
module OutJ = Semgrep_output_v1_t

let meth = "semgrep/search"

(*****************************************************************************)
(* Parameters *)
(*****************************************************************************)

module Request_params = struct
  type t = { pattern : string; lang : Xlang.t option }

  (* This schema means that it matters what order the arguments are in!
     This is a little undesirable, but it's annoying to be truly
     order-agnostic, and this is what `ocaml-lsp` also does.
     https://github.com/ocaml/ocaml-lsp/blob/ad209576feb8127e921358f2e286e68fd60345e7/ocaml-lsp-server/src/custom_requests/req_wrapping_ast_node.ml#L8
  *)
  let of_jsonrpc_params params : t option =
    match params with
    | Some (`Assoc [ ("pattern", `String pattern); ("language", lang) ]) ->
        let lang_opt =
          match lang with
          | `String lang -> Some (Xlang.of_string lang)
          | _ -> None
        in
        Some { pattern; lang = lang_opt }
    | __else__ -> None

  let _of_jsonrpc_params_exn params : t =
    match of_jsonrpc_params params with
    | None -> failwith "expected jsonrpc schema matching search"
    | Some res -> res
end

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

(** on a semgrep/search request, get the pattern and (optional) language params.
    We then try and parse the pattern in every language (or specified lang), and
    scan like normal, only returning the match ranges per file *)
let on_request runner params =
  match Request_params.of_jsonrpc_params params with
  | None -> None
  | Some params ->
      (* TODO: figure out why rules_from_rules_source_async hangs *)
      (* let src = Rules_source.(Pattern (pattern, xlang_opt, None)) in *)
      let rules_and_origins =
        Rule_fetching.rules_from_pattern (params.pattern, params.lang, None)
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
