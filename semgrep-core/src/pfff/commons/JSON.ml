module Y = Yojson.Basic

(* compatibility mode with json-wheel *)

(* a JSON object as a string *)
type str = string

type t =
  | Object of (string * t) list
  | Array of t list
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
[@@deriving show]

let rec (to_yojson: t -> Y.t) = function
  | Object xs -> `Assoc (xs |> List.map (fun (s, t) -> s, to_yojson t))
  | Array xs -> `List (xs |> List.map to_yojson)
  | String s -> `String s
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Float x when Float.is_integer x ->
      (* needed for atdgen readers that reject e.g. '4.0' when expecting
         an integer *)
      `Int (int_of_float x)
  | Float f -> `Float f
  | Null -> `Null

let rec (from_yojson: Y.t -> t) = function
  | `Assoc xs -> Object (xs |> List.map (fun (s, t) -> s, from_yojson t))
  | `List xs -> Array (xs |> List.map from_yojson)
  | `String s -> String s
  | `Int i -> Int i
  | `Bool b -> Bool b
  | `Float f -> Float f
  | `Null -> Null


let load_json file =
  let y = Y.from_file file in
  from_yojson y

let json_of_string str =
  let y = Y.from_string str in
  from_yojson y


let string_of_json ?compact ?recursive ?allow_nan json =
  ignore(compact, recursive, allow_nan);
  let y = to_yojson json in
  Y.to_string ~std:true y
