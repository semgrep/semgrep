(* Yoann Padioleau
 *
 * Copyright (C) 2014 Facebook
 * Copyright (C) 2019 r2c
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

module E = Entity_code
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Centralize error management related to errors in user's code
 * (as detected by tools such as linters).
 *
 * history:
 *  - saw something similar in the code of c--
 *  - was in check_module.ml
 *  - was generalized for scheck php
 *  - introduced ranking via int (but mess)
 *  - introduced simplified ranking using intermediate rank type
 *  - fully generalize when introduced graph_code_checker.ml
 *  - added @Scheck annotation
 *  - added some false positive deadcode detection
 *
 * todo:
 *  - priority to errors, so dead code func more important than dead field
 *  - factorize code with errors_cpp.ml, errors_php.ml, error_php.ml
*)

(*****************************************************************************)
(* Globals *)
(*****************************************************************************)
(* see g_errors below *)

(* do not report certain errors.
 * Must be used with filter_maybe_parse_and_fatal_errors.
*)
let report_parse_errors = ref false
let report_fatal_errors = ref false

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type t = {
  typ: error_kind;
  loc: Parse_info.token_location;
  sev: severity;
}
(* less: Advice | Noisy | Meticulous ? *)
(* The three-level breakdown here is based on this loose standard: *)
(* http://docs.oasis-open.org/sarif/sarif/v2.0/csprd01/sarif-v2.0-csprd01.html#_Ref493404972 *)
and severity = Error | Warning | Info

and error_kind =
  (* parsing related errors.
   * See also try_with_exn_to_errors(), try_with_error_loc_and_reraise(), and
   * filter_maybe_parse_and_fatal_errors
  *)
  | LexicalError of string
  | ParseError (* aka SyntaxError *)
  | AstBuilderError of string
  | AstGenericError of string
  | OtherParsingError of string

  (* entities *)
  (* done while building the graph:
   *  - UndefinedEntity (UseOfUndefined)
   *  - MultiDefinedEntity (DupeEntity)
  *)
  (* global analysis checker.
   * Never done by compilers, and unusual for linters to do that.
   *
   * note: OCaml 4.01 now does that partially by locally checking if
   * an entity is unused and not exported (which does not require a
   * global analysis)
  *)
  | Deadcode of entity
  | UndefinedDefOfDecl of entity
  (* really a special case of Deadcode decl *)
  | UnusedExport of entity (* tge decl*) * Common.filename (* file of def *)

  (* call sites *)
  (* should be done by the compiler (ocaml does):
   * - TooManyArguments, NotEnoughArguments
   * - WrongKeywordArguments
   * - ...
  *)

  (* variables *)
  (* also done by some compilers (ocaml does):
   * - UseOfUndefinedVariable
   * - UnusedVariable
  *)
  | UnusedVariable of string * Scope_code.t

  (* CFG/DFG.
   * Again, unreachable statements are rarely checked by compilers or linters,
   * but they really should (see https://www.wired.com/2014/02/gotofail/).
   * Those are also special cases of Deadcode.
  *)
  | UnusedStatement (* a.k.a UnreachableStatement *)
  | UnusedAssign of string
  | UseOfUninitialized of string
  | CFGError of string

  (* classes *)

  (* files (include/import) *)

  (* bail-out constructs *)
  (* a proper language should not have that *)

  (* lint *)


  (* other *)
  | FatalError of string (* missing file, OCaml errors, etc. *)
  | Timeout of string option
  | OutOfMemory of string option

(* todo: should be merged with Graph_code.entity or put in Database_code?*)
and entity = (string * Entity_code.entity_kind)

(* deprecated: alias, but you should use Error_code.t *)
type error = t

type rank =
  (* Too many FPs for now. Not applied even in strict mode. *)
  | Never
  (* Usually a few FPs or too many of them. Only applied in strict mode. *)
  | OnlyStrict
  | Less
  | Ok
  | Important
  | ReallyImportant

(* @xxx to acknowledge or explain false positives *)
type annotation =
  | AtScheck of string

(* to detect false positives (we use the Hashtbl.find_all property) *)
type identifier_index = (string, Parse_info.token_location) Hashtbl.t

(*****************************************************************************)
(* Pretty printers *)
(*****************************************************************************)

let string_of_error_kind error_kind =
  match error_kind with
  | Deadcode (s, kind) ->
      spf "dead %s, %s" (Entity_code.string_of_entity_kind kind) s
  | UndefinedDefOfDecl (s, kind) ->
      spf "no def found for %s (%s)" s (Entity_code.string_of_entity_kind kind)
  | UnusedExport ((s, kind), file_def) ->
      spf "useless export of %s (%s) (consider forward decl in %s)"
        s (Entity_code.string_of_entity_kind kind) file_def

  | UnusedVariable (name, scope) ->
      spf "Unused variable %s, scope = %s" name
        (Scope_code.string_of_scope scope)

  | UnusedStatement -> spf "unreachable statement"
  | UnusedAssign s ->
      spf "useless assignement for %s; the value in %s is never used after."
        s s
  | UseOfUninitialized s ->
      spf "use of unitialized variable: %s" s

  | LexicalError s -> spf "Lexical error: %s" s
  | ParseError -> "Syntax error"
  | AstBuilderError s -> spf "AST generation error: %s" s
  | AstGenericError s -> spf "AST generic error: %s" s
  | OtherParsingError s -> spf "Other parsing error: %s" s
  | CFGError s -> spf "Control flow error: %s" s
  | FatalError s -> spf "Fatal Error: %s" s
  | Timeout None -> "Timeout"
  | Timeout (Some s) -> "Timeout:" ^ s
  | OutOfMemory None -> "Out of memory"
  | OutOfMemory (Some s) -> "Out of memory:" ^ s

(*
let loc_of_node root n g =
  try
    let info = G.nodeinfo n g in
    let pos = info.G.pos in
    let file = Filename.concat root pos.PI.file in
    spf "%s:%d" file pos.PI.line
  with Not_found -> "NO LOCATION"
*)

let string_of_error err =
  let pos = err.loc in
  assert (pos.PI.file <> "");
  spf "%s:%d:%d: %s"
    pos.PI.file
    pos.PI.line
    pos.PI.column
    (string_of_error_kind err.typ)


(*****************************************************************************)
(* Main entry points *)
(*****************************************************************************)

let g_errors = ref []

let mk_error tok err =
  let loc = PI.unsafe_token_location_of_info tok in
  { loc = loc; typ = err; sev = Error }

let mk_error_loc loc err =
  { loc = loc; typ = err; sev = Error }

let error tok err =
  Common.push (mk_error tok err) g_errors
let warning tok err =
  let loc = PI.unsafe_token_location_of_info tok in
  Common.push { loc = loc; typ = err; sev = Warning } g_errors
let info tok err =
  let loc = PI.unsafe_token_location_of_info tok in
  Common.push { loc = loc; typ = err; sev = Info } g_errors

let error_loc loc err =
  Common.push (mk_error_loc loc err)  g_errors
let warning_loc loc err =
  Common.push { loc = loc; typ = err; sev = Warning } g_errors
let info_loc loc err =
  Common.push { loc = loc; typ = err; sev = Info } g_errors

(*****************************************************************************)
(* Ranking *)
(*****************************************************************************)

let score_of_rank = function
  | Never -> 0
  | OnlyStrict -> 1
  | Less -> 2
  | Ok -> 3
  | Important -> 4
  | ReallyImportant -> 5

let rank_of_error err =
  match err.typ with
  | Deadcode (_s, kind) ->
      (match kind with
       | E.Function -> Ok
       (* or enable when use propagate_uses_of_defs_to_decl in graph_code *)
       | E.GlobalExtern | E.Prototype -> Less
       | _ -> Ok
      )
  (* probably defined in assembly code? *)
  | UndefinedDefOfDecl _ -> Important
  (* we want to simplify interfaces as much as possible! *)
  | UnusedExport _ -> ReallyImportant
  | UnusedVariable _ -> Less
  | UnusedStatement | UnusedAssign _ | UseOfUninitialized _ -> Important
  | CFGError _ -> Important

  (* usually issues in my parsers *)
  | LexicalError _ | ParseError | AstBuilderError _ | OtherParsingError _
  | AstGenericError _
    -> OnlyStrict
  (* usually a bug somewhere in my code *)
  | FatalError _ | Timeout _ | OutOfMemory _
    -> OnlyStrict


let score_of_error err =
  err |> rank_of_error |> score_of_rank

(*****************************************************************************)
(* Error adjustments *)
(*****************************************************************************)

let options () = [
  "-report_parse_errors", Arg.Set report_parse_errors,
  " report parse errors instead of silencing them";
  "-report_fatal_errors", Arg.Set report_fatal_errors,
  " report fatal errors instead of silencing them";
]

let _is_test_or_example file =
  (file =~ ".*test.*" ||
   file =~ ".*spec.*" ||
   file =~ ".*example.*" ||
   file =~ ".*bench.*"
  )

let filter_maybe_parse_and_fatal_errors errs =
  errs |> Common.exclude (fun err ->
    let _file = err.loc.PI.file in
    match err.typ with
(*
    | LexicalError _ | ParseError
    | AstbuilderError _ | OtherParsingError _
    | FatalError _
      when is_test_or_example file -> true
*)
    | LexicalError _ | ParseError
    | AstBuilderError _ | OtherParsingError _
    | AstGenericError _
      when not !report_parse_errors -> true
    | FatalError _
      when not !report_fatal_errors -> true
    | _ -> false
  )

let exception_to_error file e =
  match Exception.get_exn e with
  | Parse_info.Lexical_error (s, tok) ->
      mk_error tok (LexicalError s)
  | Parse_info.Parsing_error tok ->
      mk_error tok (ParseError);
  | Parse_info.Ast_builder_error (s, tok) ->
      mk_error tok (AstBuilderError s);
  | Parse_info.Other_error (s, tok) ->
      mk_error tok (OtherParsingError s);
      (* TODO: can't now that AST_generic is out of h_program-lang *)
(*
  | AST_generic.Error (s, tok) ->
      mk_error tok (AstGenericError s);
      (* this should never be captured *)
      (* in theory we should also avoid to capture those *)
*)
  | Common.Timeout timeout_info ->
      (* This exception should always be reraised. *)
      let loc = Parse_info.first_loc_of_file file in
      let msg = Common.string_of_timeout_info timeout_info in
      mk_error_loc loc (Timeout (Some msg))
  | Out_of_memory ->
      let loc = Parse_info.first_loc_of_file file in
      mk_error_loc loc (OutOfMemory None)
  | (UnixExit _) as exn -> Exception.catch_and_reraise exn
  (* general case, can't extract line information from it, default to line 1 *)
  | _exn ->
      let loc = Parse_info.first_loc_of_file file in
      let msg = Exception.to_string e in
      mk_error_loc loc (FatalError msg)

let try_with_exn_to_error file f =
  try f ()
  with exn ->
    let e = Exception.catch exn in
    Common.push (exception_to_error file e) g_errors

let try_with_print_exn_and_reraise file f =
  try
    f ()
  with exn ->
    let e = Exception.catch exn in
    let err = exception_to_error file e in
    pr2 (string_of_error err);
    Exception.reraise e

let adjust_paths_relative_to_root root errs =
  errs |> List.map (fun e ->
    let file = e.loc.PI.file in
    let file' = Common.filename_without_leading_path root file in
    { e with loc = { e.loc with PI.file = file' } }
  )

(* this is for false positives *)
let adjust_errors xs =
  xs |> Common.exclude (fun err ->
    let file = err.loc.PI.file in

    match err.typ with
    | Deadcode (s, kind) ->
        (match kind with
         | E.Dir | E.File -> true

         (* kencc *)
         | E.Prototype when s = "SET" || s = "USED" -> true

         (* FP in graph_code_clang for now *)
         | E.Type when s =~ "E__anon" -> true
         | E.Type when s =~ "U__anon" -> true
         | E.Type when s =~ "S__anon" -> true
         | E.Type when s =~ "E__" -> true
         | E.Type when s =~ "T__" -> true

         (* FP in graph_code_c for now *)
         | E.Type when s =~ "U____anon" -> true

         (* TODO: to remove, but too many for now *)
         | E.Constructor
         | E.Field
           -> true

         (* hmm plan9 specific? being unused for one project does not mean
          * it's not used by another one.
         *)
         | _ when file =~ "^include/" -> true

         | _ when file =~ "^EXTERNAL/" -> true

         (* too many FP on dynamic lang like PHP *)
         | E.Method -> true

         | _ -> false
        )

    (* kencc *)
    | UndefinedDefOfDecl (("SET" | "USED"), _) -> true

    | UndefinedDefOfDecl _ ->

        (* hmm very plan9 specific *)
        file =~ "^include/" ||
        file = "kernel/lib/lib.h" ||
        file = "kernel/network/ip/ip.h" ||
        file =~ "kernel/conf/" ||
        false

    | _ -> false
  )

(*****************************************************************************)
(* Annotations *)
(*****************************************************************************)

let annotation_of_line_opt s =
  if s =~ ".*@\\([A-Za-z_]+\\):[ ]?\\([^@]*\\)"
  then
    let (kind, explain) = Common.matched2 s in
    Some (match kind with
      | "Scheck" -> AtScheck explain
      | s -> failwith ("Bad annotation: " ^ s)
    )
  else None

(* The user can override the checks by adding special annotations
 * in the code at the same line than the code it related to.
*)
let annotation_at2 loc =
  let file = loc.PI.file in
  let line = max (loc.PI.line - 1) 1 in
  match Common2.cat_excerpts file [line] with
  | [s] -> annotation_of_line_opt s
  | _ -> failwith (spf "wrong line number %d in %s" line file)

let annotation_at a =
  Common.profile_code "Errors_code.annotation" (fun () -> annotation_at2 a)

(*****************************************************************************)
(* Helper functions to use in testing code *)
(*****************************************************************************)
let default_error_regexp = ".*\\(ERROR\\|MATCH\\):"

let (expected_error_lines_of_files :
       ?regexp:string ->
     Common.filename list ->
     (Common.filename * int) (* line *) list) =
  fun ?(regexp = default_error_regexp) test_files ->
  test_files
  |> List.map (fun file ->
    Common.cat file |> Common.index_list_1
    |> Common.map_filter (fun (s, idx) ->
      (* Right now we don't care about the actual error messages. We
       * don't check if they match. We are just happy to check for
       * correct lines error reporting.
      *)
      if s =~ regexp (* + 1 because the comment is one line before *)
      then Some (file, idx + 1)
      else None))
  |> List.flatten

let compare_actual_to_expected actual_errors expected_error_lines =
  let actual_error_lines =
    actual_errors
    |> List.map (fun err ->
      let loc = err.loc in
      (loc.PI.file, loc.PI.line))
  in
  (* diff report *)
  let _common, only_in_expected, only_in_actual =
    Common2.diff_set_eff expected_error_lines actual_error_lines
  in

  only_in_expected
  |> List.iter (fun (src, l) ->
    pr2 (spf "this one error is missing: %s:%d" src l));
  only_in_actual
  |> List.iter (fun (src, l) ->
    pr2
      (spf "this one error was not expected: %s:%d (%s)" src l
         (actual_errors
          |> List.find (fun err ->
            let loc = err.loc in
            src =$= loc.PI.file && l =|= loc.PI.line)
          |> string_of_error)));
  let num_errors = List.length only_in_actual + List.length only_in_expected in
  let msg =
    spf "it should find all reported errors and no more (%d errors)" num_errors
  in
  match num_errors with
  | 0 -> Stdlib.Ok ()
  | n -> Error (n, msg)
