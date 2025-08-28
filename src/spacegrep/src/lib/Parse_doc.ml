(*
   Copyright (c) 2020-2025 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
(*
   Parse a document.
*)

module Log = Log_spacegrep.Log

let of_lexbuf (lexbuf : Lexing.lexbuf) =
  Log.debug (fun m ->
      m "parse spacegrep target file %S" lexbuf.lex_curr_p.pos_fname);
  match Parse_pattern.of_lexbuf ~is_doc:true lexbuf with
  | Ok pat -> Doc_AST.of_pattern pat
  | Error _ ->
      (* No errors when ~is_doc:true. *)
      assert false

let of_src src = Src_file.to_lexbuf src |> of_lexbuf
