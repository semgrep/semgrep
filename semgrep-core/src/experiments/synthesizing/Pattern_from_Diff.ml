module In = Input_to_core_j
module Out = Output_from_core_j
module R = Range

let range_of_ast ast = R.range_of_tokens (Visitor_AST.ii_of_any ast)

let pattern_from_diff f =
  let file = f.In.filename in
  let function_from_range file_ast range =
    let r = R.range_of_line_spec range file in
    let func = Range_to_AST.function_at_range r file_ast in
    match func with
    | None -> None
    | Some func ->
        let func_r =
          let r2_opt = range_of_ast func in
          match r2_opt with
          (* NoTokenLocation issue for the expression, should fix! *)
          | None -> failwith "No range found"
          | Some r2 -> r2
        in
        let func_str = R.content_at_range file func_r in
        Some func_str
  in
  let functions =
    try
      let file_ast = Parse_target.parse_program file in
      List.filter_map (function_from_range file_ast) f.In.diffs
    with
    | _ -> []
  in
  { Out.url = f.In.url; filename = f.In.filename; funcnames = functions }
