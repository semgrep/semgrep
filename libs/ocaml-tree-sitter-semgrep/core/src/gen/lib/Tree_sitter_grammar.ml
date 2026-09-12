(* Tree-sitter grammar specification types.

   See <https://raw.githubusercontent.com/tree-sitter/tree-sitter/refs/tags/v0.26.3/docs/src/assets/schemas/grammar.schema.json> *)

type ident = string [@@deriving ord, yojson]

type prec_value =
  | Num of int
  | Named of string
[@@deriving ord]

let prec_value_to_yojson = function
  | Num i -> `Int i
  | Named s -> `String s

let prec_value_of_yojson : Yojson.Safe.t -> (prec_value, _) Result.t  = function
  | `Int i -> Ok (Num  i)
  | `String s -> Ok (Named s)
  | _ -> Error "prec_value must be int or string"


type named_prec_level =
  | Prec_symbol of ident
  | Prec_string of string

let named_prec_level_to_yojson = function
  | Prec_symbol name ->
      `Assoc ["type", `String "SYMBOL"; "name", `String name]
  | Prec_string value ->
      `Assoc ["type", `String "STRING"; "value", `String value]

let named_prec_level_of_yojson = function
  | `Assoc fields ->
      (match List.assoc_opt "type" fields with
       | Some (`String "SYMBOL") ->
           (match List.assoc_opt "name" fields with
            | Some (`String s) -> Ok (Prec_symbol s)
            | Some _ -> Error "SYMBOL name must be string"
            | None -> Error "SYMBOL missing name field")
       | Some (`String "STRING") ->
           (match List.assoc_opt "value" fields with
            | Some (`String s) -> Ok (Prec_string s)
            | Some _ -> Error "STRING value must be string"
            | None -> Error "STRING missing value field")
       | _ -> Error "Unknown named_prec_level type")
  | _ -> Error "named_prec_level must be object"

type prec =
  | Default
  | Left
  | Right
  | Dynamic
[@@deriving ord]

(* Rule_body, Alias, and Reserved are mutually recursive. Alias and Reserved
   are modules so that they can contain records with the same field name and
   type. *)
module rec Alias : sig
  type t = {
    value: string;
    named: bool;
    content: Rule_body.t;
    must_be_preserved: bool;
  }
  [@@deriving ord, yojson]
end = struct
  type t = {
    value: string;
    named: bool;
    content: Rule_body.t;
    must_be_preserved: bool [@default false];
  }
  [@@deriving ord, yojson { strict = false }]
end

and Reserved : sig
  type t = {
    context_name: string;
    content: Rule_body.t;
  }
  [@@deriving ord, yojson]
end = struct
  type t = {
    context_name: string;
    content: Rule_body.t;
  }
  [@@deriving ord, yojson { strict = false }]
end

and Rule_body : sig
  type t =
    | Symbol of ident
    | Literal of string
    | Pattern of { value: string; flags: string option }
    | Blank
    | Repeat of t
    | Repeat1 of t
    | Choice of t list
    | Seq of t list
    | Prec of prec * prec_value * t
    | Alias of Alias.t
    | Field of ident * t
    | Immediate_token of t
    | Token of t
    | Reserved of Reserved.t
  [@@deriving ord, yojson]
end = struct
  type t =
    | Symbol of ident
    | Literal of string
    | Pattern of { value: string; flags: string option }
    | Blank
    | Repeat of t
    | Repeat1 of t
    | Choice of t list
    | Seq of t list
    | Prec of prec * prec_value * t
    | Alias of Alias.t
    | Field of ident * t
    | Immediate_token of t
    | Token of t
    | Reserved of Reserved.t
  [@@deriving ord]

  (* Prepend "type" to the fields of a derived record serialization. *)
  let with_type type_str json =
    match json with
    | `Assoc fields -> `Assoc (("type", `String type_str) :: fields)
    | _ -> assert false

  let rec to_yojson = function
    | Blank ->
        `Assoc ["type", `String "BLANK"]
    | Literal value ->
        `Assoc ["type", `String "STRING"; "value", `String value]
    | Pattern { value; flags } ->
        let fields = ["type", `String "PATTERN"; "value", `String value] in
        let fields = match flags with
          | None -> fields
          | Some f -> fields @ ["flags", `String f]
        in
        `Assoc fields
    | Symbol name ->
        `Assoc ["type", `String "SYMBOL"; "name", `String name]
    | Seq members ->
        `Assoc ["type", `String "SEQ"; "members", `List (List.map to_yojson members)]
    | Choice members ->
        `Assoc ["type", `String "CHOICE"; "members", `List (List.map to_yojson members)]
    | Alias a -> with_type "ALIAS" (Alias.to_yojson a)
    | Repeat content ->
        `Assoc ["type", `String "REPEAT"; "content", to_yojson content]
    | Repeat1 content ->
        `Assoc ["type", `String "REPEAT1"; "content", to_yojson content]
    | Reserved r -> with_type "RESERVED" (Reserved.to_yojson r)
    | Token content ->
        `Assoc ["type", `String "TOKEN"; "content", to_yojson content]
    | Immediate_token content ->
        `Assoc ["type", `String "IMMEDIATE_TOKEN"; "content", to_yojson content]
    | Field (name, content) ->
        `Assoc [
          "type", `String "FIELD";
          "name", `String name;
          "content", to_yojson content
        ]
    | Prec (type_, value, content) ->
        let prec_type = match type_ with
          | Default -> "PREC"
          | Left -> "PREC_LEFT"
          | Right -> "PREC_RIGHT"
          | Dynamic -> "PREC_DYNAMIC"
        in
        `Assoc [
          "type", `String prec_type;
          "value", prec_value_to_yojson value;
          "content", to_yojson content
        ]

  and of_yojson json =
    let open Yojson.Safe.Util in
    let ( let* ) = Result.bind in
    let content j = of_yojson (j |> member "content") in
    let parse_members ctor =
      let members = json |> member "members" |> to_list |> List.map (fun r ->
        match of_yojson r with Ok rule -> rule | Error e -> failwith e
      ) in
      Ok (ctor members)
    in
    let parse_prec type_ =
      let* value = prec_value_of_yojson (json |> member "value") in
      let* c = content json in
      Ok (Prec (type_, value, c))
    in
    try
      match json |> member "type" |> to_string with
      | "BLANK"          -> Ok Blank
      | "STRING"         -> Ok (Literal (json |> member "value" |> to_string))
      | "PATTERN"        ->
          let value = json |> member "value" |> to_string in
          let flags = json |> member "flags" |> to_string_option in
          Ok (Pattern { value; flags })
      | "SYMBOL"         -> Ok (Symbol (json |> member "name" |> to_string))
      | "SEQ"            -> parse_members (fun m -> Seq m)
      | "CHOICE"         -> parse_members (fun m -> Choice m)
      | "REPEAT"         -> let* c = content json in Ok (Repeat c)
      | "REPEAT1"        -> let* c = content json in Ok (Repeat1 c)
      | "TOKEN"          -> let* c = content json in Ok (Token c)
      | "IMMEDIATE_TOKEN"-> let* c = content json in Ok (Immediate_token c)
      | "ALIAS"          -> let* a = Alias.of_yojson json in Ok (Alias a)
      | "RESERVED"       -> let* r = Reserved.of_yojson json in Ok (Reserved r)
      | "FIELD"          ->
          let name = json |> member "name" |> to_string in
          let* c = content json in
          Ok (Field (name, c))
      | "PREC"           -> parse_prec Default
      | "PREC_LEFT"      -> parse_prec Left
      | "PREC_RIGHT"     -> parse_prec Right
      | "PREC_DYNAMIC"   -> parse_prec Dynamic
      | type_ ->
          Error (Printf.sprintf "Unknown rule type: %s" type_)
    with
    | Yojson.Safe.Util.Type_error (msg, _) ->
        Error (Printf.sprintf "Type error: %s" msg)
    | Failure msg ->
        Error msg
end

(* Helpers for (string * 'a) list fields stored as JSON objects. *)
let assoc_to_yojson f xs = `Assoc (List.map (fun (k, v) -> (k, f v)) xs)

let assoc_of_yojson (f : Yojson.Safe.t -> ('a, string) Result.t) = function
  | `Assoc obj ->
      List.fold_right (fun (k, v) acc ->
        match acc, f v with
        | Ok xs, Ok x -> Ok ((k, x) :: xs)
        | Error _ as e, _ -> e
        | _, Error e -> Error e
      ) obj (Ok [])
  | _ -> Error "Expected a JSON object"

let rule_body_list_of_yojson json =
  match Yojson.Safe.Util.to_list json |> List.map Rule_body.of_yojson with
  | exception Yojson.Safe.Util.Type_error (msg, _) -> Error msg
  | results ->
      List.fold_right (fun r acc ->
        match acc, r with
        | Ok xs, Ok x -> Ok (x :: xs)
        | Error _ as e, _ -> e
        | _, Error e -> Error e
      ) results (Ok [])

type grammar = {
  name: ident;
  inherits: ident option[@default None];
  word: ident option [@default None];
  extras: Rule_body.t list [@default []];
  inline: ident list [@default []];
  conflicts: ident list list [@default []];
  precedences: named_prec_level list list [@default []];
  externals: Rule_body.t list [@default []];
  supertypes: ident list [@default []];
  reserved: (ident * Rule_body.t list) list
            [@default []]
            [@of_yojson assoc_of_yojson rule_body_list_of_yojson]
            [@to_yojson assoc_to_yojson (fun xs -> `List (List.map Rule_body.to_yojson xs))];
  rules: (ident * Rule_body.t) list
         [@of_yojson assoc_of_yojson Rule_body.of_yojson]
         [@to_yojson assoc_to_yojson Rule_body.to_yojson];
}
[@@deriving yojson { strict = false }]
