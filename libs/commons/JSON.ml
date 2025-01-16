(* Yoann Padioleau
 *
 * Copyright (C) 1998-2025 Yoann Padioleau
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
 *)

module Y = Yojson.Basic

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(* a JSON value as a string, e.g., "\"Foobar\"", "true", "[1,2]".
 * TODO: use a JsonStr of string instead of an alias for stricter typeing?
 *)
type str = string

(* compatibility mode with json-wheel *)
type t =
  | Object of (string * t) list
  | Array of t list
  | String of string
  | Int of int
  | Float of float
  | Bool of bool
  | Null
[@@deriving show, eq]

(* polymorphic variant style, used in Yojson.Basic.t *)
type yojson =
  [ `Null
  | `Bool of bool
  | `Int of int
  | `Float of float
  | `String of string
  | `Assoc of (string * yojson) list
  | `List of yojson list ]

(* used in Ezjsonm.mli and Yaml.mli *)
type ezjsonm =
  [ `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `A of ezjsonm list
  | `O of (string * ezjsonm) list ]

(*****************************************************************************)
(* Converters *)
(*****************************************************************************)

let rec (to_yojson : t -> yojson) = function
  | Object xs -> `Assoc (xs |> List_.map (fun (s, t) -> (s, to_yojson t)))
  | Array xs -> `List (xs |> List_.map to_yojson)
  | String s -> `String s
  | Int i -> `Int i
  | Bool b -> `Bool b
  | Float x when Float.is_integer x ->
      (* needed for atdgen readers that reject e.g. '4.0' when expecting
         an integer *)
      `Int (int_of_float x)
  | Float f -> `Float f
  | Null -> `Null

let rec (from_yojson : yojson -> t) = function
  | `Assoc xs -> Object (xs |> List_.map (fun (s, t) -> (s, from_yojson t)))
  | `List xs -> Array (xs |> List_.map from_yojson)
  | `String s -> String s
  | `Int i -> Int i
  | `Bool b -> Bool b
  | `Float f -> Float f
  | `Null -> Null

let rec yojson_to_ezjsonm (json : yojson) : ezjsonm =
  match json with
  | `Assoc xs -> `O (xs |> List_.map (fun (s, t) -> (s, yojson_to_ezjsonm t)))
  | `List xs -> `A (xs |> List_.map yojson_to_ezjsonm)
  | `String s -> `String s
  | `Int i -> `Float (float_of_int i)
  | `Bool b -> `Bool b
  | `Float f -> `Float f
  | `Null -> `Null

let rec ezjsonm_to_yojson (json : ezjsonm) : yojson =
  match json with
  | `O xs -> `Assoc (xs |> List_.map (fun (s, t) -> (s, ezjsonm_to_yojson t)))
  | `A xs -> `List (xs |> List_.map ezjsonm_to_yojson)
  | `String s -> `String s
  | `Bool b -> `Bool b
  | `Float x when Float.is_integer x -> `Int (int_of_float x)
  | `Float f -> `Float f
  | `Null -> `Null

(*****************************************************************************)
(* of_string, string_of *)
(*****************************************************************************)

let json_of_string str =
  let y = Y.from_string str in
  from_yojson y

let json_of_chan (chan : Chan.i) =
  let y = Y.from_channel chan.ic in
  from_yojson y

let string_of_json ?compact ?recursive ?allow_nan json =
  ignore (compact, recursive, allow_nan);
  let y = to_yojson json in
  Y.to_string ~std:true y

(*****************************************************************************)
(* Misc *)
(*****************************************************************************)

let prettify (str : str) : str = Yojson.Basic.prettify str

let member m j =
  match j with
  | Object members ->
      List.find_map
        (fun (m', x) -> if String.equal m m' then Some x else None)
        members
  | _ -> None

(* Essentially List.merge, but with a function for how to combine elements
   which compare equal. *)
let rec merge cmp cmb xs ys =
  match (xs, ys) with
  | [], l
  | l, [] ->
      l
  | x :: xs, y :: ys ->
      let c = cmp x y in
      if c = 0 then cmb x y :: merge cmp cmb xs ys
      else if c <= 0 then x :: merge cmp cmb xs (y :: ys)
      else y :: merge cmp cmb (x :: xs) ys

let update source updates =
  match (source, updates) with
  | `Assoc xs, `Assoc ys ->
      let xs = List.sort (Common.on String.compare fst) xs in
      let ys = List.sort (Common.on String.compare fst) ys in
      `Assoc (merge (Common.on String.compare fst) (fun _ x -> x) xs ys)
  | _ -> updates

(* When a json is not a [String ...]  *)
exception NotAJString of t

(* ATDgen does not support the ability to get the string
 * of a type; you can get the json string (that is what will
 * be ultimately saved in a .json file), but not the
 * string so we need this hacky function to return the string.
 *
 * alt: could use regexp to remove the quotes
 *)
let remove_enclosing_quotes_of_jstring (str : str) : string =
  match json_of_string str with
  (* this will have the effect of removing the quote *)
  | String s -> s
  | x -> raise (NotAJString x)
