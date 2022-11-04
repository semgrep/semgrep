(* [generate_boilerplate_map_todo file] will print on stdout some
 * boilerplate code of the form:
 *
 *  let todo _env _x =
 *    failwith "TODO"
 *
 *  let rec map_expr env e =
 *    match e with
 *    | Int v1 ->
 *      let v1 = map_int env v1 in
 *      todo env v1
 *    | Plus (v1, v2) ->
 *      let v1 = map_expr env v1 in
 *      let v2 = map_expr env v2 in
 *      todo env (v1, v2)
 *    | ...
 *
 * for each OCaml type definitions in [file].
 *
 * The original boilerplate generator was:
 * https://github.com/aryx/ocamltarzan/blob/master/pa/pa_map_todo.ml
 *)
val generate_boilerplate_map_todo : Common.filename -> unit

(* helpers used also in Test_otarzan.ml *)
val parse : Common.filename -> Ast_ml.program
