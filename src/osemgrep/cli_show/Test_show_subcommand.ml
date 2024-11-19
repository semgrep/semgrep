(* Yoann Padioleau
 *
 * Copyright (C) 2024 Semgrep, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * LICENSE for more details.
 *)
open Common
open Fpath_.Operators
module F = Testutil_files

let t = Testo.create

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Testing end-to-end (e2e) the show subcommand.
 *)

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
(* alt: define Show_subcommand.caps *)
type caps = < Cap.stdout ; Cap.network ; Cap.tmp >

(* for dump-config test *)
let eqeq_basic_content =
  {|
rules:
  - id: eqeq-bad
    patterns:
      - pattern: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

(* for dump-rule-v2 test *)
let eqeq_basic_content_v2 =
  {|
rules:
  - id: eqeq-bad
    match: $X == $X
    message: "useless comparison"
    languages: [python]
    severity: ERROR
|}

let foo_py_content = {|
def foo():
    return 42
|}

(* for dump-identity *)
let with_fake_login settings_content f =
  let files = [ F.File ("settings.yml", settings_content) ] in
  F.with_tempfiles ~chdir:true ~verbose:true files (fun cwd ->
      Semgrep_envvars.with_envvar "SEMGREP_SETTINGS_FILE"
        !!(cwd / "settings.yml")
        f)

let fake_settings =
  {|
has_shown_metrics_notification: true
anonymous_user_id: f96a240e-13df-40d6-93a8-44b7fed7a569
api_token: deaddead007
|}

(* for dump-identity *)
let with_fake_identity_response return_value f =
  let make_response_fn (req : Cohttp.Request.t) _body =
    match Uri.path (Cohttp.Request.uri req) with
    | "/api/agent/identity" ->
        Http_mock_client.check_method `GET req.meth;
        let response_body = return_value |> Cohttp_lwt.Body.of_string in
        Lwt.return Http_mock_client.(basic_response response_body)
    | url -> Alcotest.fail (spf "unexpected request: %s" url)
  in
  Http_mock_client.with_testing_client make_response_fn f ()

let fake_identity = {|{"identity":"cli_fake_user_valid-from-fake-date"}|}

(* for dump-deployment
 * coupling: copy-paste of Test_login_subcommand.with_fake_deployment ...
 * but simpler to avoid a dependency to cli_login/ here.
 *)
let with_fake_deployment_response return_value f =
  let make_response_fn (req : Cohttp.Request.t) _body =
    match Uri.path (Cohttp.Request.uri req) with
    | "/api/agent/deployments/current" ->
        Http_mock_client.check_method `GET req.meth;
        let response_body = return_value |> Cohttp_lwt.Body.of_string in
        Lwt.return Http_mock_client.(basic_response response_body)
    | url -> Alcotest.fail (spf "unexpected request: %s" url)
  in
  Http_mock_client.with_testing_client make_response_fn f ()

let fake_deployment = {|{"deployment":{"id":42,"name":"fake_deployment"}}|}

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
let test_error_no_arguments (caps : caps) : Testo.t =
  t __FUNCTION__ (fun () ->
      try
        let _exit = Show_subcommand.main caps [| "semgrep-show" |] in
        failwith "semgrep show should return an exn and not reached here"
      with
      | Error.Semgrep_error
          ( "'semgrep show' expects a subcommand. Try 'semgrep show --help'.",
            None ) ->
          ())

(* TODO: how to just check that the ouptut matches Semgrep.version?
 * I don't want to create a snapshot file for it that anyway I will
 * have to mask.
 *)
(*
let test_version (caps : caps) : Testo.t =
  t ~checked_output:(Testo.stdout ()) __FUNCTION__ (fun () ->
      let exit_code =
        Show_subcommand.main caps [| "semgrep-show"; "version" |]
      in
      Exit_code.Check.ok exit_code)
*)

(* similar to test_misc.py test_cli_test_show_supported_languages *)
let test_supported_languages (caps : caps) : Testo.t =
  t ~checked_output:(Testo.stdout ()) __FUNCTION__ (fun () ->
      let exit_code =
        Show_subcommand.main caps [| "semgrep-show"; "supported-languages" |]
      in
      Exit_code.Check.ok exit_code)

let test_dump_config (caps : caps) : Testo.t =
  t ~checked_output:(Testo.stdout ())
    ~normalize:
      [
        (* because of the use of Xpattern.count global for pattern id *)
        Testo.mask_line ~after:"pid = " ~before:" }" ();
        Testo.mask_line ~after:"id_info_id = " ~before:" }" ();
      ]
    __FUNCTION__
    (fun () ->
      let files = [ F.File ("rule.yml", eqeq_basic_content) ] in
      let exit_code =
        Testutil_files.with_tempfiles ~chdir:true ~verbose:true files
          (fun _cwd ->
            Show_subcommand.main caps
              [| "semgrep-show"; "dump-config"; "rule.yml" |])
      in
      Exit_code.Check.ok exit_code)

let test_dump_rule_v2 (caps : caps) : Testo.t =
  t ~checked_output:(Testo.stdout ()) __FUNCTION__ (fun () ->
      let files = [ F.File ("rule.yml", eqeq_basic_content_v2) ] in
      let exit_code =
        Testutil_files.with_tempfiles ~chdir:true ~verbose:true files
          (fun _cwd ->
            Show_subcommand.main caps
              [| "semgrep-show"; "dump-rule-v2"; "rule.yml" |])
      in
      Exit_code.Check.ok exit_code)

(* less: could also test the dump-ast -json *)
let test_dump_ast (caps : caps) : Testo.t =
  t ~checked_output:(Testo.stdout ())
    ~normalize:
      [
        (* because of the use of GenSym.MkId.
         * TODO? note that it may not be enough to make the test
         * stable across multiple runs because if the id counter vary a lot,
         * and takes lots of integers, this could cause a reindentation
         * of the AST
         *)
        Testo.mask_line ~after:"id_info_id=" ~before:";" ();
      ]
    __FUNCTION__
    (fun () ->
      let files = [ F.File ("foo.py", foo_py_content) ] in
      let exit_code =
        Testutil_files.with_tempfiles ~chdir:true ~verbose:true files
          (fun _cwd ->
            Show_subcommand.main caps
              [| "semgrep-show"; "dump-ast"; "python"; "foo.py" |])
      in
      Exit_code.Check.ok exit_code)

let test_dump_ast_when_error (caps : caps) : Testo.t =
  t ~checked_output:(Testo.stdxxx ()) ~normalize:[ Testutil_logs.mask_time ]
    __FUNCTION__ (fun () ->
      let files = [ F.File ("error.js", "function (") ] in
      let exit_code =
        Testutil_files.with_tempfiles ~chdir:true ~verbose:true files
          (fun _cwd ->
            Show_subcommand.main caps
              [| "semgrep-show"; "dump-ast"; "error.js" |])
      in
      Exit_code.Check.invalid_code exit_code)

let test_dump_pattern (caps : caps) : Testo.t =
  t ~checked_output:(Testo.stdout ())
    ~normalize:
      [
        (* because of the use of GenSym.MkId *)
        Testo.mask_line ~after:"id_info_id=" ~before:";" ();
      ]
    __FUNCTION__
    (fun () ->
      let exit_code =
        Show_subcommand.main caps
          [| "semgrep-show"; "dump-pattern"; "python"; "foo(..., $X == $X)" |]
      in
      Exit_code.Check.ok exit_code)

let test_identity (caps : caps) : Testo.t =
  (* TODO: we use stdxxx here because we're using Logs.app for some of the output
   * instead of CapConsole in Whoami.ml, but we should really use CapConsole
   * and just capture stdout here.
   *)
  t ~checked_output:(Testo.stdxxx ()) __FUNCTION__ (fun () ->
      let exit_code =
        (* we need to be logged in otherwise we will not contact the server *)
        with_fake_login fake_settings (fun () ->
            with_fake_identity_response fake_identity (fun () ->
                Show_subcommand.main caps [| "semgrep-show"; "identity" |]))
      in
      Exit_code.Check.ok exit_code)

let test_deployment (caps : caps) : Testo.t =
  t ~checked_output:(Testo.stdxxx ()) __FUNCTION__ (fun () ->
      let exit_code =
        with_fake_login fake_settings (fun () ->
            with_fake_deployment_response fake_deployment (fun () ->
                Show_subcommand.main caps [| "semgrep-show"; "deployment" |]))
      in
      Exit_code.Check.ok exit_code)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let tests (caps : caps) =
  Testo.categorize "Osemgrep Show (e2e)"
    [
      test_error_no_arguments caps;
      (* This follows the same order than that the cases in
       * Show_CLI.show_kind (Version | SupportedLanguages | Identity | ...)
       *)
      (*      test_version caps; *)
      test_supported_languages caps;
      test_identity caps;
      test_deployment caps;
      test_dump_pattern caps;
      test_dump_ast caps;
      test_dump_ast_when_error caps;
      test_dump_config caps;
      test_dump_rule_v2 caps;
      (* TODO? engine_path and command_for_core *)
    ]
