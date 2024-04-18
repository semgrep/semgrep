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
module F = Testutil_files
module TL = Test_login_subcommand

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

(* for --dump-config test *)
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

(* for --dump-rule-v2 test *)
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

(*****************************************************************************)
(* Tests *)
(*****************************************************************************)
let test_error_no_arguments (caps : caps) : Testo.test =
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
let test_version (caps : caps) : Testo.test =
  t ~checked_output:(Testo.stdout ()) __FUNCTION__ (fun () ->
      let exit_code =
        Show_subcommand.main caps [| "semgrep-show"; "version" |]
      in
      Exit_code.Check.ok exit_code)
*)

(* similar to test_misc.py test_cli_test_show_supported_languages *)
let test_supported_languages (caps : caps) : Testo.test =
  t ~checked_output:(Testo.stdout ()) __FUNCTION__ (fun () ->
      CapConsole.out caps#stdout (spf "Snapshot for %s" __FUNCTION__);
      let exit_code =
        Show_subcommand.main caps [| "semgrep-show"; "supported-languages" |]
      in
      Exit_code.Check.ok exit_code)

let test_dump_config (caps : caps) : Testo.test =
  t ~checked_output:(Testo.stdout ())
    ~normalize:
      [
        (* because of the use of Xpattern.count global for pattern id *)
        Testo.mask_line ~after:"pid = " ~before:" }" ();
      ]
    __FUNCTION__
    (fun () ->
      CapConsole.out caps#stdout (spf "Snapshot for %s" __FUNCTION__);
      let files = [ F.File ("rule.yml", eqeq_basic_content) ] in
      let exit_code =
        Testutil_files.with_tempfiles ~chdir:true ~verbose:true files
          (fun _cwd ->
            Show_subcommand.main caps
              [| "semgrep-show"; "dump-config"; "rule.yml" |])
      in
      Exit_code.Check.ok exit_code)

let test_dump_rule_v2 (caps : caps) : Testo.test =
  t ~checked_output:(Testo.stdout ()) __FUNCTION__ (fun () ->
      CapConsole.out caps#stdout (spf "Snapshot for %s" __FUNCTION__);
      let files = [ F.File ("rule.yml", eqeq_basic_content_v2) ] in
      let exit_code =
        Testutil_files.with_tempfiles ~chdir:true ~verbose:true files
          (fun _cwd ->
            Show_subcommand.main caps
              [| "semgrep-show"; "dump-rule-v2"; "rule.yml" |])
      in
      Exit_code.Check.ok exit_code)

(* less: could also test the dump-ast -json *)
let test_dump_ast (caps : caps) : Testo.test =
  t ~checked_output:(Testo.stdout ())
    ~normalize:
      [
        (* because of the use of GenSym.MkId *)
        Testo.mask_line ~after:"id_info_id=" ~before:";" ();
      ]
    __FUNCTION__
    (fun () ->
      CapConsole.out caps#stdout (spf "Snapshot for %s" __FUNCTION__);
      let files = [ F.File ("foo.py", foo_py_content) ] in
      let exit_code =
        Testutil_files.with_tempfiles ~chdir:true ~verbose:true files
          (fun _cwd ->
            Show_subcommand.main caps
              [| "semgrep-show"; "dump-ast"; "python"; "foo.py" |])
      in
      Exit_code.Check.ok exit_code)

let test_dump_pattern (caps : caps) : Testo.test =
  t ~checked_output:(Testo.stdout ())
    ~normalize:
      [
        (* because of the use of GenSym.MkId *)
        Testo.mask_line ~after:"id_info_id=" ~before:";" ();
      ]
    __FUNCTION__
    (fun () ->
      CapConsole.out caps#stdout (spf "Snapshot for %s" __FUNCTION__);
      let exit_code =
        Show_subcommand.main caps
          [| "semgrep-show"; "dump-pattern"; "python"; "foo(..., $X == $X)" |]
      in
      Exit_code.Check.ok exit_code)

let test_identity (caps : caps) : Testo.test =
  (* TODO: use stdxxx here because we're using Logs.app for some of the output
   * instead of CapConsole in Whoami.ml
   *)
  t ~checked_output:(Testo.stdxxx ()) __FUNCTION__ (fun () ->
      CapConsole.out caps#stdout (spf "Snapshot for %s" __FUNCTION__);
      let exit_code =
        Show_subcommand.main caps [| "semgrep-show"; "identity" |]
      in
      Exit_code.Check.ok exit_code)

let test_deployment (caps : caps) : Testo.test =
  t ~checked_output:(Testo.stdxxx ()) __FUNCTION__ (fun () ->
      CapConsole.out caps#stdout (spf "Snapshot for %s" __FUNCTION__);
      let exit_code =
        TL.with_fake_deployment_response TL.fake_deployment (fun () ->
            Show_subcommand.main caps [| "semgrep-show"; "deployment" |])
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
      test_dump_config caps;
      test_dump_rule_v2 caps;
      (* TODO? engine_path and command_for_core *)
    ]
