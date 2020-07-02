open Common
module J = Json_type

let synthesize_patterns s file =
  let r = Range.range_of_linecol_spec s file in
  let ast = Parse_generic.parse_program file in
  let lang = Lang.langs_of_filename file |> List.hd in
  let e_opt = Range_to_AST.expr_at_range r ast in
  Naming_AST.resolve lang ast;
  match e_opt with
  | Some e ->
       let patterns = Pattern_from_Code.from_expr e in
       List.map (fun (k, v) -> (k, Pretty_print_generic.pattern_to_string lang v)) patterns
  | None -> failwith (spf "could not find an expr at range %s in %s" s file)
