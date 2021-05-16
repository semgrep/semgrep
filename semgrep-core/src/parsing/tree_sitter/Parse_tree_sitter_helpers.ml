(* Yoann Padioleau
 *
 * Copyright (C) 2020 r2c
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
module PI = Parse_info

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* A few helpers to help factorize code between the different
 * Parse_xxx_tree_sitter.ml
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)
type 'a env = {
  file : Common.filename;
  (* get the charpos (offset) in file given a line x col *)
  conv : (int * int, int) Hashtbl.t;
  extra : 'a;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* mostly a copy of Parse_info.full_charpos_to_pos_large *)
let line_col_to_pos file =
  let chan = open_in file in
  let size = Common2.filesize file + 2 in

  let charpos = ref 0 in
  let line = ref 0 in
  let h = Hashtbl.create size in

  let full_charpos_to_pos_aux () =
    try
      while true do
        let s = input_line chan in
        incr line;

        (* '... +1 do'  cos input_line dont return the trailing \n *)
        for i = 0 to String.length s - 1 + 1 do
          Hashtbl.add h (!line, i) (!charpos + i)
        done;
        charpos := !charpos + String.length s + 1
      done
    with End_of_file ->
      (* bugfix: this is wrong:  Hashtbl.add h (!line, 0) !charpos;
       * because an ident on the last line would get
       * the last charpos.
       *)
      ()
  in
  full_charpos_to_pos_aux ();
  close_in chan;
  h

let token env (tok : Tree_sitter_run.Token.t) =
  let loc, str = tok in
  let h = env.conv in
  let start = loc.Tree_sitter_run.Loc.start in
  (* Parse_info is 1-line based and 0-column based, like Emacs *)
  let line = start.Tree_sitter_run.Loc.row + 1 in
  let column = start.Tree_sitter_run.Loc.column in
  let charpos =
    try Hashtbl.find h (line, column) with Not_found -> -1
    (* TODO? more strict? raise exn? *)
  in
  let file = env.file in
  let tok_loc = { PI.str; charpos; line; column; file } in
  { PI.token = PI.OriginTok tok_loc; transfo = PI.NoTransfo }

let str env (tok : Tree_sitter_run.Token.t) =
  let _, s = tok in
  (s, token env tok)

let combine_tokens_DEPRECATED env xs =
  match xs with
  | [] -> failwith "combine_tokens: empty list"
  | x :: _xsTODO ->
      let t = token env x in
      t

let int_of_string_c_octal_opt s =
  if s =~ "^0\\([0-7]+\\)$" then
    let s = Common.matched1 s in
    int_of_string_opt ("0o" ^ s)
  else int_of_string_opt s

let wrap_parser tree_sitter_parser ast_mapper =
  (* Note that because we currently use Parallel.invoke to
   * invoke the tree-sitter parser, unmarshalled exn
   * can't be used in match or try or used for structural equality.
   * So take care! Fortunately the ocaml-tree-sitter parsers now
   * return a list of error instead of an exception so this is now
   * less an issue.
   *)
  let res : 'a Tree_sitter_run.Parsing_result.t = tree_sitter_parser () in
  let program =
    match res.program with Some cst -> Some (ast_mapper cst) | None -> None
  in
  { res with program }

(* Stuff to put in entry point at the beginning:
   let todo _env _x = failwith "not implemented"

   let todo_any str t any =
   pr2 (AST.show_any any);
   raise (Parse_info.Ast_builder_error (str, t))

   let program =
   ...
   try
   program ...
   with
    (Failure "not implemented") as exn ->
      (* This debugging output is not JSON and breaks core output
       *
       * let s = Printexc.get_backtrace () in
       * pr2 "Some constructs are not handled yet";
       * pr2 "CST was:";
       * CST.dump_tree ast;
       * pr2 "Original backtrace:";
       * pr2 s;
       *)
      raise exn

*)
