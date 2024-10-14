(*
   Print function definitions how we like them.
*)

open Printf
open AST_ocaml

(*****************************************************************************)
(* Pretty-printing *)
(*****************************************************************************)

(*
   Line: single line of text, indented as needed.
   Block: Add extra indentation.
   Inline: groups multiple items into one like Block but without introducing
           extra indentation like Line. This is convenient to avoids calls to
           List.flatten or (@) when combining pieces of generated code.
*)
type out = Line of string | Block of out list | Inline of out list

(* Generic pretty-printer *)
let print (xs : out list) =
  let rec print indent = function
    | Line "" -> printf "\n"
    | Line line -> printf "%s%s\n" indent line
    | Inline xs -> print_list indent xs
    | Block xs -> print_list (indent ^ "  ") xs
  and print_list indent xs = List.iter (print indent) xs in
  print_list "" xs

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let str_of_ident (s, _) = s

(* TODO: report line position at some point where the error occurs *)
let error s = failwith s

let intersperse sep xs =
  let rec aux = function
    | [] -> []
    | [ x ] -> [ x ]
    | x :: xs -> x :: sep :: aux xs
  in
  aux xs

let insert_blank_lines xs = intersperse (Line "") xs

(* Associate a local variable name to each element of a list:
    [x] -> [(x, "v")]
    [foo; bar] -> [(foo, "v1"); (bar, "v2")]
*)
let arg_names_of_list xs =
  match xs with
  | [] -> []
  | [ x ] -> [ (x, "v") ]
  | xs -> List_.mapi (fun i x -> (x, sprintf "v%i" (i + 1))) xs

(* Create tuple pattern or expression:
     "()"
     "v"
     "(v1, v2, v3)"
*)
let tuple_of_list ?(use_unit = true) ?(spaced = false) xs =
  let vars = arg_names_of_list xs |> List_.map snd in
  let space = if spaced then " " else "" in
  match vars with
  | [] -> if use_unit then space ^ "()" else ""
  | [ var ] -> space ^ var
  | _ -> sprintf "%s(%s)" space (String.concat ", " vars)

let case_args_of_list xs = tuple_of_list ~use_unit:false ~spaced:true xs

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let func_for_str_name (conf : Conf.t) (name : string) : string =
  conf.fun_prefix ^ name

let func_for_name (conf : Conf.t) (name : name) : string =
  match name with
  | [], id -> sprintf "%s%s" conf.fun_prefix (str_of_ident id)
  | qu, id ->
      let prefix = qu |> List_.map str_of_ident |> String.concat "." in
      sprintf "%s.%s%s" prefix conf.fun_prefix (str_of_ident id)

(*
   Return the name of the function to call for a given type.
   If the type doesn't have a name, we create a local definition and
   return it. This function definition will have to be inserted just
   before the function call.

   Example for an anonymous tuple:

    | Foo of (a * b)
             ^^^^^^^
   ->

    | Foo v1 ->
        let map_local (v1, v2) =
          (map_a v1, map_b v2)
        in
        Foo (map_local v1)

   In this case, func_for_type would have returned (def, "map_local")
   where def is the definition of map_local.
*)
let rec func_for_type (conf : Conf.t) (typ : type_) : out list * string =
  match typ with
  | TyName name -> ([], func_for_name conf name)
  | TyVar _ -> ([], "TODOTyVar")
  | TyAny _ -> error "TyAny not handled"
  | TyFunction _ -> ([], "TODOTyFunction")
  | TyApp ((_, [ ty ], _), name) ->
      let s1 = func_for_name conf name in
      let def, s2 = func_for_type conf ty in
      (def, sprintf "(%s %s)" s1 s2)
  | TyApp (_tys, _name) -> ([], "TodoTyAppManyArgs")
  | TyTuple types ->
      assert (List.length types >= 2);
      let local_name = "tuple" in
      let def =
        gen_typedef conf true
          (TyDecl
             {
               tname = (local_name, Tok.unsafe_sc);
               tparams = None;
               tbody = CoreType typ;
             })
      in
      let local_def = [ Inline def; Line "in" ] in
      (local_def, func_for_str_name conf local_name)
  (* should be handled in CoreType case in gen_typedef below *)
  | TyEllipsis _ -> error "TyEllipsis impossible"
  | TyTodo ((s, _), _ty) -> ([], sprintf "TODOTyTodo_%s" s)

(*
   Generate a top-level group of mutually-recursive function definitions
   from a group of type definitions.
   The syntax is such that appending "in" turns it into a local definition.

     let rec map_foo env x =
       ...
     and map_bar env x =
       ...
*)
and gen_typedef (conf : Conf.t) is_first typedef : out list =
  match typedef with
  | TyDecl { tname; tparams = _TODO; tbody } ->
      let name = str_of_ident tname in
      let arg_pat, body =
        match tbody with
        | AbstractType -> error "AbstractType not handled"
        | AlgebraicType ctors ->
            let cases = List_.map (gen_case conf) ctors in
            ("v", [ Line "match v with"; Inline cases ])
        | RecordType (_open, fields, _close) -> ("v", gen_record conf fields)
        | CoreType typ -> (
            match typ with
            | TyTuple tys ->
                let arg_pat = tuple_of_list tys in
                (arg_pat, gen_tuple conf tys)
            | _ ->
                let def, func = func_for_type conf typ in
                ("v", [ Inline def; Line (sprintf "%s env v" func) ]))
        | TdTodo _ -> error "TdTodo not handled"
      in
      [
        Line
          (sprintf "%s %s%s env %s ="
             (if is_first then "let rec" else "and")
             conf.fun_prefix name arg_pat);
        Block body;
      ]
  | TyDeclTodo _ -> error "TyDeclTodo not handled"

and gen_case conf (id, xs) =
  let n_args = List.length xs in
  let xs = arg_names_of_list xs in
  let arg_handlers, construct =
    match conf.format with
    | Map ->
        let arg_handlers =
          xs
          |> List_.map (fun (typ, var) ->
                 let def, call_str = func_for_type conf typ in
                 Inline
                   [
                     Inline def;
                     Line (sprintf "let %s = %s env %s in" var call_str var);
                   ])
        in
        let construct = Line (sprintf "todo env %s" (tuple_of_list xs)) in
        (arg_handlers, construct)
    | Visit ->
        let is_last idx = idx = n_args - 1 in
        let arg_handlers =
          xs
          |> List_.mapi (fun i (typ, var) ->
                 let def, call_str = func_for_type conf typ in
                 Inline
                   [
                     Inline def;
                     Line
                       (sprintf "%s env %s%s" call_str var
                          (if is_last i then "" else ";"));
                   ])
        in
        let construct =
          match xs with
          | [] -> Line "()"
          | _ -> Inline []
        in
        (arg_handlers, construct)
    | Compare ->
        (* TODO *)
        ([], Inline [])
  in
  Inline
    [
      Line (sprintf "| %s%s ->" (str_of_ident id) (case_args_of_list xs));
      Block [ Block [ Inline arg_handlers; construct ] ];
    ]

and gen_record conf fields =
  let field_names =
    fields |> List_.map (fun ((name, _), _, _) -> name) |> String.concat "; "
  in
  let result =
    match conf.format with
    | Map ->
        [
          Inline
            (List_.map
               (fun ((name, _), type_, _mut) ->
                 let def, func = func_for_type conf type_ in
                 Inline
                   [
                     Inline def;
                     Line (sprintf "let %s = %s env %s in" name func name);
                   ])
               fields);
          Line (sprintf "{%s}" field_names);
        ]
    | Visit ->
        List_.map
          (fun ((name, _), type_, _mut) ->
            let def, func = func_for_type conf type_ in
            Inline [ Inline def; Line (sprintf "%s env %s;" func name) ])
          fields
    | Compare ->
        (* TODO *)
        [ Line "(* TODO *)" ]
  in
  [ Line (sprintf "let {%s} = v in" field_names); Inline result ]

and gen_tuple conf types =
  let types = arg_names_of_list types in
  let tuple = tuple_of_list types in
  match conf.format with
  | Map ->
      [
        Inline
          (types
          |> List_.map (fun (type_, var) ->
                 let def, call_str = func_for_type conf type_ in
                 Inline
                   [
                     Inline def;
                     Line (sprintf "let %s = %s env %s in" var call_str var);
                   ]));
        Line (sprintf "todo env %s" tuple);
      ]
  | Visit ->
      let len = List.length types in
      types
      |> List_.mapi (fun i (type_, var) ->
             let def, call_str = func_for_type conf type_ in
             Inline
               [
                 Inline def;
                 Line
                   (sprintf "%s env %s%s" call_str var
                      (if i < len - 1 then ";" else ""));
               ])
  | Compare -> [ Line "TODO" ]

let gen_typedef_group conf typedefs =
  let is_first =
    let is_first = ref true in
    fun () ->
      if !is_first then (
        is_first := false;
        true)
      else false
  in
  (* List_.map would work but doesn't guarantee left-to-right evaluation :-( *)
  List.fold_left
    (fun acc x -> Inline (gen_typedef conf (is_first ()) x) :: acc)
    [] typedefs
  |> List.rev

(* port of https://github.com/aryx/ocamltarzan/blob/master/pa/pa_map_todo.ml *)
let generate_boilerplate conf typedef_groups =
  let body =
    typedef_groups
    |> List_.map (fun x ->
           Inline (gen_typedef_group conf x |> insert_blank_lines))
    |> insert_blank_lines
  in
  let prelude = [ Line "let todo _env _v = failwith \"TODO\""; Line "" ] in
  let everything = prelude @ body in
  print everything
