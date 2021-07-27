(*
   Parse a document.
*)

let of_lexbuf lexbuf =
  match Parse_pattern.of_lexbuf ~is_doc:true lexbuf with
  | Ok pat -> Doc_AST.of_pattern pat
  | Error _ ->
      (* No errors when ~is_doc:true. *)
      assert false

let of_src src = Src_file.to_lexbuf src |> of_lexbuf
