(* Yoann Padioleau
 *
 * Copyright (C) 2011 Facebook
 * Copyright (C) 2019 r2c
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License (GPL)
 * version 2 as published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * file license.txt for more details.
 *)
open Common

module R = Rule

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)
let error s =
  failwith (spf "sgrep_lint: wrong format. %s" s)

let severity_of_string = function
 | "ERROR" -> R.Error
 | "WARNING" -> R.Warning
 | s -> error (spf "Bad severity: %s" s)

(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let parse file =
  let str = Common.read_file file in
  let yaml_res = Yaml.of_string str in
  match yaml_res with
  | Result.Ok v ->
      (match v with
      | `O ["rules", `A xs] ->
         xs |> List.map (fun v ->
          match v with
          | `O xs ->
            (match Common.sort_by_key_lowfirst xs with
            | [
            "id", `String id;
            "languages", `A langs;
            "message", `String message;
            "pattern", `String pattern;
            "severity", `String sev;
            ] ->
               let languages = langs |> List.map (function
                | `String s ->
                  (match Lang.lang_of_string_opt s with
                  | None -> error (spf "unsupported language: %s" s)
                  | Some l -> l
                  )
                | _ -> error (spf "expecting a string for languages")
               ) in
               let lang =
                 match languages with
                 | [] -> error "we need at least one language"
                 | x::_xs -> x
               in
               let pattern =
                 (* todo? call Normalize_ast.normalize here? *)
                 try Parse_generic.parse_pattern lang pattern
                 with exn ->
                   error (spf "could not parse the pattern %s (exn = %s)"
                            pattern (Common.exn_to_s exn))
               in
               let severity = severity_of_string sev in
               { R. id; pattern; message; languages; severity }
             | x ->
               pr2_gen x;
               error "wrong rule fields"
             )
          | x ->
              pr2_gen x;
              error "wrong rule fields"
         )
      | _ -> error "missing rules entry"
      )
  | Result.Error (`Msg s) ->
    failwith (spf "sgrep_lint: could not parse %s (error = %s)" file s)

(*
      let sgrep_string = Common.matched1 s in
      let title, msg = match group with
        | title :: description -> title, Common2.unlines description
        | _ -> failwith ("sgrep_lint: expected \"[title]\\n\\n[description]\"")
      in
      Parse_generic.parse_pattern !lang sgrep_string,
      title,
      (* yes ocaml regexps are not that good ... *)
      (if msg =~ "^\\([A-Z]+\\):\\(\\(.\\|\n\\)*\\)"
       then
         let (error_kind, rest_msg) = Common.matched2 msg in
         (match error_kind with
         | _ -> failwith ("sgrep_lint: wrong format: " ^ msg)
         )
        else failwith ("sgrep_lint: wrong format: " ^ msg)
      )
    else raise Impossible
  )
*)