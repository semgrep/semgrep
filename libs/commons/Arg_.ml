(* Yoann Padioleau
 *
 * Copyright (C) 1998-2023 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Pad's extensions to Arg for actions. See pfff's Main.ml for
 * an example of use.
 *
 * DEPRECATED: this module is deprecated, you should use the
 * Cmdliner library instead.
 *
 * todo? or just use Cmdliner ...
 *  - isn't unison or scott-mcpeak-lib-in-cil handles that kind of
 *    stuff better ? That is the need to localize command line argument
 *    while still being able to gathering them. Same for logging.
 *    Similiar to the type prof = PALL | PNONE | PSOME of string list.
 *    Same spirit of fine grain config in log4j ?
 *  - how mercurial/cvs/git manage command line options ? because they
 *    all have a kind of DSL around arguments with some common options,
 *    specific options, conventions, etc.
 *  - generate the corresponding noxxx options ?
 *  - generate list of options and show their value ?
 *  - make it possible to set this value via a config file ?
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type arg_spec_full = Arg.key * Arg.spec * Arg.doc
type cmdline_options = arg_spec_full list

(* the format is a list of triples:
 *  (title of section * (optional) explanation of sections * options)
 *)
type options_with_title = string * string * arg_spec_full list
type cmdline_sections = options_with_title list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let (lines : string -> string list) =
 fun s ->
  let rec lines_aux = function
    | [] -> []
    | [ x ] -> if x = "" then [] else [ x ]
    | x :: xs -> x :: lines_aux xs
  in
  Str.split_delim (Str.regexp "\n") s |> lines_aux

(*****************************************************************************)
(* Entry points *)
(*****************************************************************************)

(* now I use argv as I like at the call sites to show that
 * this function internally use argv.
 *)
let parse_options options usage_msg argv =
  let args = ref [] in
  try
    Arg.parse_argv argv options (fun file -> args := file :: !args) usage_msg;
    args := List.rev !args;
    !args
  with
  | Arg.Bad msg ->
      Printf.eprintf "%s" msg;
      raise (UnixExit 2)
  | Arg.Help msg ->
      UPrintf.printf "%s" msg;
      raise (UnixExit 0)

let usage usage_msg options = Arg.usage (Arg.align options) usage_msg

(* for coccinelle *)

(* If you don't want the -help and --help that are appended by Arg.align *)
let arg_align2 xs = Arg.align xs |> List.rev |> List_.drop 2 |> List.rev
let short_usage usage_msg ~short_opt = usage usage_msg short_opt

let pr_xxxxxxxxxxxxxxxxx () =
  UCommon.pr
    "-----------------------------------------------------------------------"

let long_usage usage_msg ~short_opt ~long_opt =
  UCommon.pr usage_msg;
  UCommon.pr "";
  let all_options_with_title = ("main options", "", short_opt) :: long_opt in
  all_options_with_title
  |> List.iter (fun (title, explanations, xs) ->
         UCommon.pr title;
         pr_xxxxxxxxxxxxxxxxx ();
         if explanations <> "" then (
           UCommon.pr explanations;
           UCommon.pr "");
         arg_align2 xs
         |> List.iter (fun (key, _action, s) -> UCommon.pr ("  " ^ key ^ s));
         UCommon.pr "");
  ()

(* copy paste of Arg.parse. Don't want the default -help msg *)
let arg_parse2 l msg short_usage_fun =
  let args = ref [] in
  let f file = args := file :: !args in
  let l = Arg.align l in
  try
    Arg.parse_argv USys.argv l f msg;
    args := List.rev !args;
    !args
  with
  | Arg.Bad msg ->
      (* eprintf "%s" msg; exit 2; *)
      let xs = lines msg in
      (* take only head, it's where the error msg is *)
      (* nosemgrep: no-logs-in-library *)
      Logs.err (fun m -> m "%s" (List_.hd_exn "unexpected empty list" xs));
      short_usage_fun ();
      raise (UnixExit 2)
  | Arg.Help _msg ->
      (* printf "%s" msg; exit 0; *)
      raise Impossible (* -help is specified in speclist *)

(* ---------------------------------------------------------------------- *)

type flag_spec = Arg.key * Arg.spec * Arg.doc

type action_spec = Arg.key * Arg.doc * action_func
and action_func = string list -> unit

type cmdline_actions = action_spec list

exception WrongNumberOfArguments

let options_of_actions action_ref actions =
  actions
  |> List_.map (fun (key, doc, _func) ->
         (key, Arg.Unit (fun () -> action_ref := key), doc))

let (action_list : cmdline_actions -> Arg.key list) =
 fun xs -> List_.map (fun (a, _b, _c) -> a) xs

let (do_action : Arg.key -> string list (* args *) -> cmdline_actions -> unit) =
 fun key args xs ->
  let assoc = xs |> List_.map (fun (a, _b, c) -> (a, c)) in
  let action_func = List.assoc key assoc in
  action_func args

(* todo? if have a function with default argument ? would like a
 *  mk_action_0_or_1_arg ?
 *)

let mk_action_0_arg f = function
  | [] -> f ()
  | _ -> raise WrongNumberOfArguments

let mk_action_1_arg f = function
  | [ file ] -> f file
  | _ -> raise WrongNumberOfArguments

let mk_action_2_arg f = function
  | [ file1; file2 ] -> f file1 file2
  | _ -> raise WrongNumberOfArguments

let mk_action_3_arg f = function
  | [ file1; file2; file3 ] -> f file1 file2 file3
  | _ -> raise WrongNumberOfArguments

let mk_action_4_arg f = function
  | [ file1; file2; file3; file4 ] -> f file1 file2 file3 file4
  | _ -> raise WrongNumberOfArguments

let mk_action_n_arg f = f
let mk_action_1_conv conv f = mk_action_1_arg (fun str -> f (conv str))
let mk_action_n_conv conv f = mk_action_n_arg (fun xs -> f (List_.map conv xs))
