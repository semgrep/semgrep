(*
   Tags used to filter tests.
*)

let todo_js = "todo:js"

(* Mandatory registration of all the tags used on our tests. *)
let () = List.iter Alcotest_ext.declare_tag [ todo_js ]

let tags_of_lang (lang : Lang.t) =
  (* Tag tests that will be skipped by the JS implementation: *)
  match lang with
  | Cpp (* TODO: investigate C++ test issues *)
  | Julia (* TODO: re-enable once we fix Julia build slowness *)
  | Ruby (* TODO: re-enable once we fix Ruby build slowness *)
  | Go (* TODO: re-enable this when we fix the jsoo int overflow bug *)
  | C (* TODO: investigate c_array_inits pattern parse error*) ->
      [ todo_js ]
  | _ -> []

let tags_of_langs (langs : Lang.t list) =
  langs |> Common.map tags_of_lang |> Common.flatten |> Common.sort
  |> Common.uniq_by ( = )
