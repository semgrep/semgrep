#!/usr/bin/env ocaml

#use "topfind"

#require "ppx_deriving.show"

#require "ppx_deriving_cmdliner"

#load "unix.cma"

#load "threads/threads.cma"

#require "feather"

#require "logs"

#require "commons"

open Common
open Cmdliner
module F = Feather
open Feather (* for |. *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Report the remaining lines of code (LOC) in pysemgrep to port and upload
 * the count to dashboard.semgrep.dev/metrics/semgrep.pysemgrep.loc
 *
 * usage:
 *   $ ./report_pysemgrep_loc --upload cli/semgrep/
 *
 * alternatives:
 *  - a bash script, like in report_test_metrics.sh
 *
 * See also ../tools/hello_script.ml.
 *)

(*****************************************************************************)
(* Types and constants *)
(*****************************************************************************)

let host = "https://dashboard.semgrep.dev"
let metric = "semgrep.pysemgrep.loc"

(* see https://github.com/hammerlab/ppx_deriving_cmdliner *)
type conf = {
  path : string; [@pos 0] [@docv "CMD"]
  upload : bool; [@default false]
  verbose : bool; [@default false]
}
[@@deriving cmdliner, show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let upload conf loc =
  let url = spf "%s/api/metric/%s" host metric in
  (* TODO: use Logs library *)
  Logs.debug (fun m -> m "uploading to %s" url);

  let cmd =
    F.process "curl"
      [ "--fail"; "-L"; "-X"; "POST"; url; "-d"; string_of_int loc ]
  in
  F.run cmd

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run conf =
  if conf.verbose then Logs.set_level (Some Logs.Debug);
  Logs.debug (fun m -> m "debug: params = %s" (show_conf conf));

  let out =
    (* alt: could also use F.find and F.grep builtins *)
    F.process "find" [ conf.path; "-type"; "f"; "-name"; "*.py" ]
    (* alt: could also use F.filter_lines *)
    |. F.process "grep" [ "-v"; "semgrep_interfaces/" ]
    |. F.process "grep" [ "-v"; "semdep/" ]
    |. F.process "xargs" [ "wc"; "-l"; "--total=only" ]
    |> F.collect F.stdout
  in
  let loc = int_of_string out in
  Logs.app (fun m ->
      m "LOC in %s (without semgrep_interfaces and semdep) = %d" conf.path loc);
  if conf.upload then upload conf loc

(*****************************************************************************)
(* Cmdliner boilerplate *)
(*****************************************************************************)
let main () =
  Fmt_tty.setup_std_outputs ();
  Logs.set_reporter (Logs_fmt.reporter ());
  let info = Cmd.info Sys.argv.(0) in
  let term = Term.(const run $ conf_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmd.eval cmd)

let () = main ()
