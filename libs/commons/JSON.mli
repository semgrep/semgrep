(* a JSON value as a string, e.g., "\"Foobar\"", "true", "[1,2]" *)
type str = string

(* old-school way to define JSON *)
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

val member : string -> t -> t option
(** [`member s j`] gives `Some v` where `v` is the value associated with the
  * key `s` in the JSON Object `j` or `None` if no such value exists (either
  * `j` is not an object or no such key is present). *)

(* converters *)
val to_yojson : t -> yojson
val from_yojson : yojson -> t
val yojson_to_ezjsonm : yojson -> ezjsonm
val ezjsonm_to_yojson : ezjsonm -> yojson

(* string_of, of_string *)
val string_of_json :
  ?compact:bool -> ?recursive:bool -> ?allow_nan:bool -> t -> str
(** NOTE: compact, recursive, allow_nan all currently unused *)

val json_of_string : str -> t
val json_of_chan : Chan.i -> t

(* alias for Yojson.Basic.prettify *)
val prettify : str -> str

val update : yojson -> yojson -> yojson
(** [update old new] makes a new JSON value by, in the case of an object [old]
    and [new], recursively [update]-ing fields from [old] with fields of the
    same name in [new], or, in other cases, using [new] directly.

    For example,
      - [update (`Int i) (`Bool b)] is [`Bool b]
      - [update (`Assoc ["a", `Int i; "c", `String s])
                (`Assoc ["a", `Bool b; "b", `Int k])] is
        [`Assoc ["a", `Bool b; "b", `Int k; "c", `String s]]
    *)

(* When a json is not a [String ...]  *)
exception NotAJString of t

(* ex: "\"Foobar\"" -> "Foobar"
 * may raise NotAJString if str does not parse into a [String ...].
 * may raise parse error if str is not a json string.
 *)
val remove_enclosing_quotes_of_jstring : str -> string
