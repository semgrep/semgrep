open Common
module J = JSON

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* There was no 'pysemgrep show' subcommand. Dumps were run via
 * 'semgrep scan --dump-ast ...' but it is better to have a separate
 * subcommand. Note that the legacy 'semgrep scan --dump-xxx' are
 * redirected to this file after having built a compatible Show_CLI.conf
 *
 * LATER: get rid of Core_CLI.dump_pattern and Core_CLI.dump_ast functions
 *
 * Note that we're using Common.pr() here, to print on stdout (Logs.app
 * is printing on stderr, but for a show command it's probably better to
 * print on stdout).
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* copy paste of Core_CLI.json_of_v *)
let json_of_v (v : OCaml.v) =
  let rec aux v =
    match v with
    | OCaml.VUnit -> J.String "()"
    | OCaml.VBool v1 -> if v1 then J.String "true" else J.String "false"
    | OCaml.VFloat v1 -> J.Float v1 (* ppf "%f" v1 *)
    | OCaml.VChar v1 -> J.String (spf "'%c'" v1)
    | OCaml.VString v1 -> J.String v1
    | OCaml.VInt i -> J.Int i
    | OCaml.VTuple xs -> J.Array (Common.map aux xs)
    | OCaml.VDict xs -> J.Object (Common.map (fun (k, v) -> (k, aux v)) xs)
    | OCaml.VSum (s, xs) -> (
        match xs with
        | [] -> J.String (spf "%s" s)
        | [ one_element ] -> J.Object [ (s, aux one_element) ]
        | _ :: _ :: _ -> J.Object [ (s, J.Array (Common.map aux xs)) ])
    | OCaml.VVar (s, i64) -> J.String (spf "%s_%d" s (Int64.to_int i64))
    | OCaml.VArrow _ -> failwith "Arrow TODO"
    | OCaml.VNone -> J.Null
    | OCaml.VSome v -> J.Object [ ("some", aux v) ]
    | OCaml.VRef v -> J.Object [ ("ref@", aux v) ]
    | OCaml.VList xs -> J.Array (Common.map aux xs)
    | OCaml.VTODO _ -> J.String "VTODO"
  in
  aux v

(* mostly a copy paste of Core_CLI.dump_v_to_format *)
let dump_v_to_format ~json (v : OCaml.v) =
  if json then J.string_of_json (json_of_v v) else OCaml.string_of_v v

(*****************************************************************************)
(* Main logic *)
(*****************************************************************************)

let run (conf : Show_CLI.conf) : Exit_code.t =
  match conf.target with
  (* TODO? error management? improve error message for parse errors?
   * or let CLI.safe_run do the right thing?
   *)
  | Pattern (str, lang) ->
      (* mostly a copy paste of Core_CLI.dump_pattern *)
      let any = Parse_pattern.parse_pattern lang ~print_errors:true str in
      let v = Meta_AST.vof_any any in
      let s = dump_v_to_format ~json:conf.json v in
      Common.pr s;
      Exit_code.ok
  | File (file, lang) ->
      (* mostly a copy paste of Core_CLI.dump_ast *)
      let { Parsing_result2.ast; skipped_tokens = _; _ } =
        Parse_target.just_parse_with_lang lang (Fpath.to_string file)
      in
      let v = Meta_AST.vof_any (AST_generic.Pr ast) in
      (* 80 columns is too little *)
      Format.set_margin 120;
      let s = dump_v_to_format ~json:conf.json v in
      Common.pr s;
      Exit_code.ok
  | Config config_str ->
      let settings = Semgrep_settings.load () in
      let token_opt = settings.api_token in
      let in_docker = !Semgrep_envvars.v.in_docker in
      let config = Rules_config.parse_config_string ~in_docker config_str in
      let rules_and_origins =
        Rule_fetching.rules_from_dashdash_config ~token_opt
          ~registry_caching:false config
      in
      rules_and_origins
      |> List.iter (fun x -> Common.pr (Rule_fetching.show_rules_and_origin x));
      Exit_code.ok
  | EnginePath _pro -> failwith "TODO: dump-engine-path not implemented yet"
  | CommandForCore -> failwith "TODO: dump-command-for-core not implemented yet"
  | SupportedLanguages ->
      Common.pr (spf "supported languages are: %s" Xlang.supported_xlangs);
      Exit_code.ok

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)
let main (argv : string array) : Exit_code.t =
  let conf = Show_CLI.parse_argv argv in
  run conf
