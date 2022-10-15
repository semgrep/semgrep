#!/usr/bin/env ocaml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Toy hello-world style script written in OCaml showing it's actually
 * possible to write scripts in OCaml!
 *
 * It also shows how to leverage a few useful libraries and ppx extensions
 * for scripting:
 *  - topfind (to easily load packages)
 *  - ppx_deriving (who doesn't like deriving show?)
 *  - ppx_cmdliner (cmdliner for mere mortals)
 *  - feather (shell DSL)
 *  - some pfff libraries! (I need my Common)
 *
 * usage:
 *   $ ./hello_script.ml --verbose world
 *   debug: params = { Hello_script.username = "pad"; verbose = true; command = "world" }
 *   Hello world from pad
 *   PAD      1869469  0.0  0.1 194628 32616 PTS/0    SL+  17:03   0:00 /HOME/PAD/.OPAM/4.14.0/BIN/OCAMLRUN /HOME/PAD/.OPAM/4.14.0/BIN/OCAML ./SCRIPTS/HELLO_SCRIPT.ML --VERBOSE WORLD
 *   PAD      1869485  0.0  0.0   6512  2572 PTS/0    S+   17:03   0:00 GREP OCAML
 *
 * alternatives:
 *  - ocamlscript?
 *
 * references:
 *  - https://discuss.ocaml.org/t/how-to-run-ml-ocaml-file-without-compiling/4311/21
 *  - https://discuss.ocaml.org/t/running-ocaml-scripts/10228
 *  - https://discuss.ocaml.org/t/ocaml-scripting-and-ppx-support/10638
 *)

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* This directive allows then to use #require to load opam/findlib packages. *)
#use "topfind"

(* You need first to 'opam install ppx_deriving'. *)
#require "ppx_deriving.show"

(* You need first to 'opam install ppx_deriving_cmdliner'. *)
#require "ppx_deriving_cmdliner"

#load "unix.cma"

#load "threads/threads.cma"

(* You need first to 'opam install feather'.
 * For some unknown reasons, requiring feather does not load the unix and
 * threads dependencies, so I had to do it manually above.
 *)
#require "feather"

(* This assumes you did a 'make install' of semgrep-core so that
 * the pfff (and semgrep-core) libraries are available from topfind.
 *)
#require "commons"

open Common
open Cmdliner
module F = Feather
open Feather (* for |. *)
open Feather.Infix (* for >, <, ||., etc. *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* see https://github.com/hammerlab/ppx_deriving_cmdliner *)
type cli_params = {
  username : string; [@default "pad"]
  verbose : bool; [@default false]
  command : string; [@pos 0] [@docv "CMD"]
}
[@@deriving cmdliner, show]

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* see https://github.com/charlesetc/feather *)
let simple_shell_programming_demo () =
  let out =
    F.process "ps" [ "-aux" ]
    |. F.map_lines ~f:String.uppercase_ascii
    |. F.process "grep" [ "OCAML" ]
    (* alt: |. F.grep "OCAML" *)
    |> F.collect F.stdout
  in
  pr out

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let run ps =
  if ps.verbose then pr2 (spf "debug: params = %s" (show_cli_params ps));
  pr (spf "Hello %s from %s" ps.command ps.username);
  simple_shell_programming_demo ();
  ()

(*****************************************************************************)
(* Cmdliner boilerplate *)
(*****************************************************************************)
let main () =
  let info = Cmd.info Sys.argv.(0) in
  let term = Term.(const run $ cli_params_cmdliner_term ()) in
  let cmd = Cmd.v info term in
  exit (Cmd.eval cmd)

let () = main ()
