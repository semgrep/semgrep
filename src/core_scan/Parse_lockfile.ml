(* TODO: Move this to a sensible place *)

open Common

let rec find_lockfile file : Fpath.t option =
  let exception Found of Fpath.t in
  let dir = Fpath.parent file in
  if Fpath.is_root dir then None
  else
    try
      dir
      |> List_files.iter (fun file _ ->
             if Fpath.basename file =*= "package-lock.json" then
               raise (Found file));
      find_lockfile dir
    with
    | Found file -> Some file

type package_lock_name =
  | Str of string * package_lock_name
  | NM of package_lock_name
  | Done

let read_key key (expr : AST_generic.expr) =
  match expr.e with
  | Container (Dict, xs) ->
      xs |> Tok.unbracket
      |> List_.find_some_opt (fun (expr : AST_generic.expr) ->
             match expr.e with
             | Container
                 ( Tuple,
                   (_, [ { e = L (String (_, (str, _), _)); _ }; value ], _) )
               when String.equal str key ->
                 Some value
             | _ -> None)
  | _ -> None

let parse_package_lock (expr : AST_generic.expr) : Supply_chain.dependency list
    =
  let ( let* ) = Option.bind in
  let rec parse_package_name str = function
    | 'n' :: 'o' :: 'd' :: 'e' :: '_' :: 'm' :: 'o' :: 'd' :: 'u' :: 'l' :: 'e'
      :: 's' :: '/' :: xs -> (
        match str with
        | _ :: _ ->
            Str
              ( Common2.string_of_list Common2.string_of_char (List.rev str),
                NM (parse_package_name [] xs) )
        | [] -> NM (parse_package_name [] xs))
    | x :: xs -> parse_package_name (x :: str) xs
    | [] -> (
        match str with
        | _ :: _ -> Str (str |> List.rev |> List.to_seq |> String.of_seq, Done)
        | [] -> Done)
  in
  let rec name_of_package_name = function
    | NM (Str (name, Done)) -> name
    | NM x
    | Str (_, x) ->
        name_of_package_name x
    | Done -> failwith "impossible"
  in
  let rec nm_count = function
    | NM x -> 1 + nm_count x
    | Str (_, x) -> nm_count x
    | Done -> 0
  in
  match read_key "packages" expr with
  | Some pckgs -> (
      let manifest_deps =
        let* manifest = read_key "" pckgs in
        read_key "dependencies" manifest
      in
      match pckgs.e with
      | Container (Dict, pckgs) ->
          pckgs |> Tok.unbracket
          |> List_.map_filter (fun (expr : AST_generic.expr) ->
                 match expr.e with
                 | Container
                     ( Tuple,
                       (_, [ { e = L (String (_, ("", _), _)); _ }; _ ], _) ) ->
                     None
                 | Container
                     ( Tuple,
                       ( _,
                         [
                           { e = L (String (_, (name, nameTok), _)); _ };
                           package;
                         ],
                         _ ) ) ->
                     let name =
                       name |> Common2.list_of_string |> parse_package_name []
                     in
                     let* package_version =
                       let* v = read_key "version" package in
                       match v.e with
                       | L (String (_, (v, _), _)) -> Some v
                       | _ -> None
                     in
                     let package_name = name |> name_of_package_name in
                     let is_nested = nm_count name > 1 in
                     Some
                       Supply_chain.
                         {
                           package_name;
                           package_version;
                           ecosystem = Npm;
                           transitivity =
                             (if is_nested then Transitive
                              else
                                match manifest_deps with
                                | Some expr -> (
                                    match read_key package_name expr with
                                    | Some _ -> Direct
                                    | None -> Transitive)
                                | _ -> Unknown);
                           url = None;
                           loc =
                             ( Tok.unsafe_loc_of_tok nameTok,
                               Tok.unsafe_loc_of_tok nameTok );
                           toks = [ nameTok ];
                         }
                 | _ -> None)
      | _ -> [])
  | None -> []

let parse_lockfile file =
  let ast, _ =
    Parse_with_caching.parse_and_resolve_name AST_generic.version Language.Json
      file
  in
  match ast with
  | [ { s = ExprStmt (e, _); _ } ] -> parse_package_lock e
  | _ -> []
