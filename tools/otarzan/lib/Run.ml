(*
   Entry point to the program and command-line interface
*)

(* [run file] will print on stdout some
 * boilerplate code of the form:
 *
 *  let todo _env _x =
 *    failwith "TODO"
 *
 *  let rec map_expr env v =
 *    match v with
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
let run (conf : Conf.t) =
  let defs = Parse.extract_typedefs_from_ml_file conf.input_file in
  Print.generate_boilerplate conf defs
