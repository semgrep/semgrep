(*
   Tags used to filter tests.

   Tags are shared by Testo and Testo_lwt.
*)

open Common

(* Should this be "js.todo"? Feel free to change it. *)
let todo_js = Testo.Tag.declare "todo.js"

(* "lang.none" would be shorter but possibly confusing since we're using
   the term "generic" everywhere. *)
let lang_generic = Testo.Tag.declare "lang.generic"
let lang_tags = Hashtbl.create 100

let make_lang_tag lang =
  let tag = spf "lang.%s" (Lang.to_lowercase_alnum lang) |> Testo.Tag.declare in
  Hashtbl.add lang_tags lang tag

let get_lang_tag lang =
  match Hashtbl.find_opt lang_tags lang with
  | None -> assert false
  | Some tag -> tag

let () =
  Language.list |> List.iter (fun (x : Language.info) -> make_lang_tag x.id)

let tags_of_lang (lang : Lang.t) =
  let lang_tags = [ get_lang_tag lang ] in
  (* Tag tests that will be skipped by the JS implementation: *)
  let todo_js_tags =
    match lang with
    | Cpp (* TODO: investigate C++ test issues *)
    | Go (* TODO: re-enable this when we fix the jsoo int overflow bug *)
    | Ql (* TODO: Doesn't work in js for some reason *)
    | C (* TODO: investigate c_array_inits pattern parse error*) ->
        [ todo_js ]
    | _ -> []
  in
  List_.flatten [ lang_tags; todo_js_tags ]

let tags_of_langs (langs : Lang.t list) =
  match langs with
  | [] -> [ lang_generic ]
  | langs ->
      langs |> List_.map tags_of_lang |> List_.flatten
      |> List.sort Testo.Tag.compare
      |> List_.deduplicate_gen ~get_key:Testo.Tag.to_string
