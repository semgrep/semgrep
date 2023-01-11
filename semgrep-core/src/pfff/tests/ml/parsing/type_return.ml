let string_of_hint_type (h : hint_type option) : string = match h with
  | Some x ->
      (match x with
       | Hint c ->
           (match c with
            | ClassName c -> Ast.name c
            | Self _ -> "self"
            | Parent _ -> "parent"
            | LateStatic _ -> ""
           )
       | HintArray _ -> "array"
      )
  | None -> ""
