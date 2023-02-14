(*
   Abstract type for a file path within a git project
*)

type t = {
  string: string;
  components: string list;
}

let of_string string = {
  string;
  components = String.split_on_char '/' string;
}

let to_string x = x.string

let check_component str =
  if String.contains str '/' then
    invalid_arg
      ("Git_path.create: path component may not contain a slash: " ^ str)

let create components =
  List.iter check_component components;
  {
    string = String.concat "/" components;
    components;
  }

let append path comp =
  check_component comp;
  {
    string = path.string ^ "/" ^ comp;
    components = path.components @ [comp];
  }

module Ops = struct
  let ( / ) = append
end

let components x = x.components

let is_absolute x =
  match x.components with
  | "" :: _ -> true
  | __else__ -> false

let is_relative x = not (is_absolute x)

let normalize x =
  let rec normalize xs =
    match xs with
    | ".." :: xs -> ".." :: normalize xs
    | ("." | "") :: xs -> normalize xs
    | _ :: ".." :: xs -> normalize xs
    | x :: xs as orig ->
        let res = normalize xs in
        (* If nothing changes via normalization, return the original list *)
        if (==) res xs then
          orig
        else
          (* Something changed, make another pass *)
          normalize (x :: res)
    | [] -> []
  in
  match x.components with
  | "" :: xs ->
      (match normalize xs with
       | ".." :: _ -> Error ("invalid git path: " ^ x.string)
       | [] -> Ok (create [""; ""])
       | components -> Ok (create ("" :: components)))
  | xs ->
      let components =
        match normalize xs with
        | [] -> ["."]
        | xs -> xs
      in
      Ok (create components)

let of_fpath path =
  Fpath.segs path |> create

let root = of_string "/"
