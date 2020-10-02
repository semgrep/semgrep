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
type env = {
    file: Common.filename;
    (* get the charpos (offset) in file given a line x col *)
    conv: (int * int, int) Hashtbl.t;
}

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

(* mostly a copy of Parse_info.full_charpos_to_pos_large *)
let line_col_to_pos = fun file ->

    let chan = open_in file in
    let size = Common2.filesize file + 2 in

    let charpos   = ref 0 in
    let line  = ref 0 in
    let h = Hashtbl.create size in

    let full_charpos_to_pos_aux () =
      try
        while true do begin
          let s = (input_line chan) in
          incr line;

          (* '... +1 do'  cos input_line dont return the trailing \n *)
          for i = 0 to (String.length s - 1) + 1 do
            Hashtbl.add h (!line, i) (!charpos + i);
          done;
          charpos := !charpos + String.length s + 1;
        end done
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
  let (loc, str) = tok in
  let h = env.conv in
  let start = loc.Tree_sitter_run.Loc.start in
  (* Parse_info is 1-line based and 0-column based, like Emacs *)
  let line = start.Tree_sitter_run.Loc.row + 1 in
  let column = start.Tree_sitter_run.Loc.column in
  let charpos =
    try Hashtbl.find h (line, column)
    with Not_found -> -1 (* TODO? more strict? raise exn? *)
  in
  let file = env.file in
  let tok_loc = { PI. str; charpos; line; column; file; } in
  { PI.token = PI.OriginTok tok_loc; transfo = PI.NoTransfo }

let str env (tok : Tree_sitter_run.Token.t) =
  let (_, s) = tok in
  s, token env tok

let combine_tokens env xs =
  match xs with
  | [] -> failwith "combine_tokens: empty list"
  | x::_xsTODO ->
      let t = token env x in
      t

let mk_tree_sitter_error (err : Tree_sitter_run.Tree_sitter_error.t) =
  let start = err.start_pos in
  let loc = {
    PI.str = err.substring;
    charpos = 0; (* fake *)
    line = start.row + 1;
    column = start.column;
    file = err.file.name;
  } in
  loc

let convert_tree_sitter_exn_to_pfff_exn f =
  try f ()
  with
  (* The case below is what we would like to do! However if
   * you use Parallel.invoke to invoke the tree-sitter parser, this
   * code below will never trigger. Indeed, unmarshalled exn
   * can't be used in match or try or used for structural equality
   * hence the ugly workaround below. See marshal.mli or Paralle.ml for
   * more information.
   *)
  | Tree_sitter_run.Tree_sitter_error.Error ts_error ->
    let loc = mk_tree_sitter_error ts_error in
    let info = { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo } in
    raise (PI.Parsing_error info)

  (* !!!UGLY!!! remove this once we don't use Paralle.invoke *)
  | exn ->
      let s = Common.exn_to_s exn in
      if s = "Tree_sitter_run.Tree_sitter_error.Error(_)" then begin
        let t = Obj.repr exn in
        let info = Obj.field t 1 in
        let (ts_error : Tree_sitter_run.Tree_sitter_error.t) = Obj.obj info in
        let loc = mk_tree_sitter_error ts_error in
        let info = { PI.token = PI.OriginTok loc; transfo = PI.NoTransfo } in
        raise (PI.Parsing_error info)
      end else raise exn


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
