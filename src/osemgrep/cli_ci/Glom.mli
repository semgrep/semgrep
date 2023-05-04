(** The Glom module is a re-implementation of the Python [glom] library.

    The goal of this library is to able to load a JSON file (see {!val:cli}) and
    get values from it with OCaml types. For instance, from this given JSON
    value:

    {[
      {
        "foo": "bar",
        "count": 42,
        "nested": { "bar": "foo" }
        "list": [ 0.1, 0.2 ]
      }
    ]}

    The user can get values with:

    {[
      open Glom

      let value_of_foo = get_and_coerce_opt string json [k"foo"]
      let value_of_bar = get_and_coerce_opt string json [k"nested"; k"bar"]
      let value_of_count = get_and_coerce_opt int json [k"count"]
      let _0_2 = get_and_coerce_opt float json [k"list"; n 1]
    ]}
*)

type t = Yojson.Basic.t

(** Cmdliner helpers. *)

val cli : (string * t) Cmdliner.Arg.conv
val default : string * t

type value =
  [ `String of string | `Bool of bool | `Float of float | `Int of int | `Null ]

type elt
and path = elt list

type 'a ty

(** Basic types. *)

val string : string ty
val bool : bool ty
val float : float ty
val int : int ty
val unit : unit ty

(** Elements of a path. *)

val k : string -> elt
val n : int -> elt

(** Accessors. *)

val coerce : 'a ty -> value -> 'a option
val get : t -> path -> value
val get_opt : t -> path -> value option
val get_and_coerce_opt : 'a ty -> t -> path -> 'a option
