(*
   Print function definitions how we like them.
*)

open Printf
open Ast_ml

(*****************************************************************************)
(* Pretty-printing *)
(*****************************************************************************)

type out = Line of string | Inline of out list | Block of out list

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

(* generate a string with a list of vars: " (v1, v2, v3, ...)"
 * and a function that can be called later with and idx to get the varname
 *)
let vxxx_of_list xs =
  let n_args = List.length xs in
  let xs = Common.index_list_1 xs in
  (* v1, v2, v3, ... *)
  let var i = if n_args = 1 then "v" else sprintf "v%d" i in
  let args =
    match xs with
    | [] -> ""
    | [ _ ] -> " " ^ var 1
    | _ ->
        sprintf " (%s)"
          (xs |> Common.map snd
          |> Common.map (fun i -> sprintf "v%d" i)
          |> String.concat ", ")
  in
  (args, n_args, xs, var)

(*****************************************************************************)
(* Entry point *)
(*****************************************************************************)

let func_for_name (conf : Conf.t) (name : name) : string =
  match name with
  | [], id -> sprintf "%s%s" conf.fun_prefix (str_of_ident id)
  | qu, id ->
      let prefix = qu |> Common.map str_of_ident |> String.concat "." in
      sprintf "%s.%s%s" prefix conf.fun_prefix (str_of_ident id)

let rec func_for_type (conf : Conf.t) (typ : type_) : string =
  match typ with
  | TyName name -> func_for_name conf name
  | TyVar _ -> "TODOTyVar"
  | TyAny _ -> error "TyAny not handled"
  | TyFunction _ -> "TODOTyFunction"
  | TyApp ([ ty ], name) ->
      let s1 = func_for_name conf name in
      let s2 = func_for_type conf ty in
      sprintf "(%s %s)" s1 s2
  | TyApp (_tys, _name) -> "TodoTyAppManyArgs"
  (* should be handled in CoreType case in gen_typedef below *)
  | TyTuple _tys -> "TodoTyTuple"
  | TyEllipsis _ -> error "TyEllipsis impossible"
  | TyTodo ((s, _), _ty) -> sprintf "TODOTyTodo_%s" s

let gen_typedef (conf : Conf.t) is_first typedef =
  match typedef with
  | TyDecl { tname; tparams = _TODO; tbody } ->
      let name = str_of_ident tname in
      let body =
        match tbody with
        | AbstractType -> error "AbstractType not handled"
        | AlgebraicType ctors ->
            let cases =
              ctors
              |> Common.map (fun (id, xs) ->
                     let args, n_args, xs, var = vxxx_of_list xs in
                     let arg_handlers, construct =
                       match conf.format with
                       | Map ->
                           let arg_handlers =
                             xs
                             |> Common.map (fun (typ, idx) ->
                                    let call_str = func_for_type conf typ in
                                    Line
                                      (sprintf "let %s = %s env %s in" (var idx)
                                         call_str (var idx)))
                           in
                           let construct = Line (sprintf "todo env%s" args) in
                           (arg_handlers, construct)
                       | Visit ->
                           let is_last idx = idx = n_args in
                           let arg_handlers =
                             xs
                             |> Common.map (fun (typ, idx) ->
                                    let call_str = func_for_type conf typ in
                                    Line
                                      (sprintf "%s env %s%s" call_str (var idx)
                                         (if is_last idx then "" else ";")))
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
                         Line (sprintf "| %s%s ->" (str_of_ident id) args);
                         Block [ Block [ Inline arg_handlers; construct ] ];
                       ])
            in
            [ Line "match v with"; Inline cases ]
        | RecordType _ -> [ Line "TODO: RecordType" ]
        | CoreType typ -> (
            match typ with
            | TyTuple tys ->
                let args, _n_args, xs, var = vxxx_of_list tys in
                [
                  Line (sprintf "(fun env %s ->" args);
                  Block
                    ((xs
                     |> Common.map (fun (typ, idx) ->
                            let call_str = func_for_type conf typ in
                            Line
                              (sprintf "let %s = %s env %s in" (var idx)
                                 call_str (var idx))))
                    @ [ Line (sprintf "todo env%s" args) ]);
                  Line ") env v";
                ]
            | _ -> [ Line (sprintf "%s env v" (func_for_type conf typ)) ])
        | TdTodo _ -> error "TdTodo not handled"
      in
      [
        Line
          (sprintf "%s %s%s env v ="
             (if is_first then "let rec" else "and")
             conf.fun_prefix name);
        Block body;
      ]
  | TyDeclTodo _ -> error "TyDeclTodo not handled"

let gen_typedef_group conf typedefs =
  let is_first =
    let is_first = ref true in
    fun () ->
      if !is_first then (
        is_first := false;
        true)
      else false
  in
  (* Common.map would work but doesn't guarantee left-to-right evaluation :-( *)
  List.fold_left
    (fun acc x -> Inline (gen_typedef conf (is_first ()) x) :: acc)
    [] typedefs
  |> List.rev

(* port of https://github.com/aryx/ocamltarzan/blob/master/pa/pa_map_todo.ml *)
let generate_boilerplate conf typedef_groups =
  let body =
    typedef_groups
    |> Common.map (fun x ->
           Inline (gen_typedef_group conf x |> insert_blank_lines))
    |> insert_blank_lines
  in
  let prelude = [ Line "let todo _env _v = failwith \"TODO\""; Line "" ] in
  let everything = prelude @ body in
  print everything
