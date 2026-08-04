(* ATD adapter for rule_schema_v2.atd
 * See https://atd.readthedocs.io/en/latest/atdgen.html#field-adapter-ocaml
 *)

module Formula = struct

  (** Convert from original json to ATD-compatible json *)
  let normalize (orig : Yojson.Safe.t ) : Yojson.Safe.t =
    match orig with
    | `String str ->
        `Assoc ["pattern", `String str]
    (* TODO: check at least one of any/all/... is specified *)
    | x -> x

  (** Convert from ATD-compatible json to original json *)
  let restore  (_atd : Yojson.Safe.t) : Yojson.Safe.t =
    (* not needed for now; we care just about parsing *)
    failwith "Rule_schema_v2_adapter.Formula.restore not implemented"
end

module Condition = struct

  let normalize (orig : Yojson.Safe.t ) : Yojson.Safe.t =
    match orig with
    | `Assoc (("comparison", cmp)::rest) ->
       `List [`String "C";
            `Assoc (("comparison", cmp)::rest)]
    | `Assoc (("metavariable", mvar)::rest) ->
       (* TODO: check at least one of type/types/... is specified *)
       `List [`String "M";
            `Assoc (("metavariable", mvar)::rest)]
    (* alt: we could do the String vs List in a separate adapter *) 
    | `Assoc [("focus", `String x)] ->
       `List [`String "F";
            `Assoc [("focus", `List [`String x])]]
    | `Assoc [("focus", `List x)] ->
       `List [`String "F";
            `Assoc [("focus", `List x)]]
    | x -> x

  let restore  (_atd : Yojson.Safe.t) : Yojson.Safe.t =
    failwith "Rule_schema_v2_adapter.Condition.restore not implemented"
end

module BySideEffect = struct

  let normalize (orig : Yojson.Safe.t ) : Yojson.Safe.t =
    match orig with
    | `Bool true -> `String "true"
    | `Bool false -> `String "false"
    | x -> x

  let restore  (_atd : Yojson.Safe.t) : Yojson.Safe.t =
    failwith "Rule_schema_v2_adapter.BySideEffect.restore not implemented"
end

module ProjectDependsOn = struct

  let normalize (orig : Yojson.Safe.t ) : Yojson.Safe.t =
    match orig with
    | `Assoc [("depends-on-either", arr)] ->
       `List [`String "E";
            `Assoc [("depends-on-either", arr)]]
    | `Assoc (xs) ->
       `List [`String "B";
            `Assoc xs]
    | x -> x

  let restore  (_atd : Yojson.Safe.t) : Yojson.Safe.t =
    failwith "Rule_schema_v2_adapter.ProjectDependsOn.restore not implemented"
end

(* This is the name of the field that contains the variant constructor
   in the user-friendly YAML convention we use to represent variants.
   See 'normalize_variant'. *)
let kind_field_name = "kind"

(*
   A generic representation for variants. The parameters, if any, must be
   an ATD record (JSON object, Yojson assoc).

     type t = [
       | A <json name="a">
       | B <json name="b"> of b
     ]

     type b = {
       (* all the fields are optional *)
       ?k: int option;
     }

   1. OCaml A is represented as JSON "A". The adapter doesn't change it.
   2. OCaml B {k = 42} is represented as JSON {"kind": "B", "k": 42}
      which the adapter converts to JSON ["kind", {"k", 42}].

   Additionally, the alternate notations {"kind": "A"} and "B" can be
   supported in addition to "A" and {"kind": "B"}. This requires specifying
   the constructors for which the alternate notation is supported.
   Constructors that don't expect an argument must be listed as 'enum'.
   Constructors that expect an object argument must be listed as 'obj'.
   This gives us the following call:

     normalize_generic_variant ~enum:["a"] ~obj:["b"] json

   Without specifying 'enum' or 'obj', YAML/JSON interpretation will be
   stricter by not tolerating the alternate notations {"kind": "A"} or "B".

   YAML example:

     - a

     - kind: b
       k: 42

     # assuming default properties:
     - kind: b

     # shorthand for {kind: b}:
     - b

     # long form for "a":
     - kind: a

   TODO: make the ATD tools (atdgen, atdpy, ...) support these alternate
   formats as well?
   This would allow us to make adapters generic i.e. without
   having to specify the 'enum' and 'obj' options. In the example above,
   atdgen would read "b" as ["b", {}] and would read ["a", {}] or ["a", null]
   as "a" without complaining.
*)
let normalize_variant
    ?(enum = [])
    ?(obj = [])
    (orig : Yojson.Safe.t ) : Yojson.Safe.t =
  match orig with
  | `Assoc props ->
      (match List.partition (fun (k, _v) -> k = kind_field_name) props with
       | [_, `String kind], [] when List.mem kind enum -> `String kind
       | [_, `String kind], other_fields ->
           `List [`String kind; `Assoc other_fields]
       | _missing_or_duplicate_kind, _ -> orig
      )
  | `String kind when List.mem kind obj -> `List [`String kind; `Assoc []]
  | _string_or_malformed -> orig

(* Unlike 'normalize_variant', this if fully generic.
   (because we're going from a strict format to a looser format) *)
let restore_variant
  (atd : Yojson.Safe.t ) : Yojson.Safe.t =
  match atd with
  | `String _ as str -> str
  | `List [`String _ as kind; `Assoc fields] ->
      `Assoc ((kind_field_name, kind) :: fields)
  | _malformed -> atd

module Analyzer = struct
  let normalize orig =
    normalize_variant
      ~enum:["entropy"; "redos"]
      ~obj:["entropy_v2"]
      orig

  let restore (atd : Yojson.Safe.t) : Yojson.Safe.t =
    restore_variant atd
end
