open Common

type t = Yojson.Basic.t

let cli =
  let parser str =
    if USys.file_exists str then
      try Ok (str, Yojson.Basic.from_file ~fname:str str) with
      | Yojson.Json_error _ -> Error (`Msg (spf "Invalid JSON file: %s" str))
    else Error (`Msg (spf "%s does not exist" str))
  in
  let show (filename, _) = filename in
  Cmdliner.Arg.conv ~docv:"<JSON file>" (parser, Fmt_.of_show show)

let default = (String.empty, `Null)

type elt = K of string | N of int
type path = elt list

type value =
  [ `String of string | `Bool of bool | `Float of float | `Int of int | `Null ]

type 'a ty =
  | String : string ty
  | Bool : bool ty
  | Float : float ty
  | Int : int ty
  | Unit : unit ty

let k key = K key
let n n = N n
let string = String
let bool = Bool
let float = Float
let int = Int
let unit = Unit

let coerce : type a. a ty -> value -> a option =
 fun k v ->
  match (k, v) with
  | String, `String v -> Some v
  | Bool, `Bool v -> Some v
  | Float, `Float v -> Some v
  | Int, `Int v -> Some v
  | Unit, `Null -> Some ()
  | __else__ -> None

let rec get json path =
  match (json, path) with
  | (#value as value), [] -> value
  | `Assoc lst, K key :: rest -> (
      match List.assoc_opt key lst with
      | Some json -> get json rest
      | None -> raise Not_found)
  | `List lst, N n :: rest -> (
      match List.nth_opt lst n with
      | Some json -> get json rest
      | None -> raise Not_found)
  | __else__ -> raise Not_found

let get_opt json path =
  try Some (get json path) with
  | Not_found -> None

let get_and_coerce_opt ty json path =
  Option.bind (get_opt json path) (coerce ty)
