(*
   Parse a document.
*)

let of_lexbuf lexbuf =
  Parse_pattern.of_lexbuf ~is_doc:true lexbuf |> Doc_AST.of_pattern

let of_src src = Src_file.to_lexbuf src |> of_lexbuf
