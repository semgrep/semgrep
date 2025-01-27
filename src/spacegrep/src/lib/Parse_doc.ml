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
