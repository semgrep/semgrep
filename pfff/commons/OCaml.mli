(*s: pfff/commons/OCaml.mli *)
(* 
 * OCaml hacks to support reflection (works with ocamltarzan).
 *
 * See also sexp.ml, json.ml, and xml.ml for other "reflective" techniques.
 *)

(*s: type [[OCaml.t]] *)
(* OCaml core type definitions (no objects, no modules) *)
type t =
  | Unit 
  | Bool | Float | Char | String | Int

  | Tuple of t list
  | Dict of (string * [`RW|`RO] * t) list (* aka record *)
  | Sum of (string * t list) list         (* aka variants *)

  | Var of string
  | Poly of string
  | Arrow of t * t

  | Apply of string * t

  (* special cases of Apply *) 
  | Option of t
  | List of t 

  | TTODO of string
(*e: type [[OCaml.t]] *)

(*s: signature [[OCaml.add_new_type]] *)
val add_new_type: string -> t -> unit
(*e: signature [[OCaml.add_new_type]] *)
(*s: signature [[OCaml.get_type]] *)
val get_type: string -> t
(*e: signature [[OCaml.get_type]] *)

(*s: type [[OCaml.v]] *)
(* OCaml values (a restricted form of expressions) *)
type v = 
  | VUnit 
  | VBool of bool | VFloat of float | VInt of int
  | VChar of char | VString of string

  | VTuple of v list
  | VDict of (string * v) list
  | VSum of string * v list

  | VVar of (string * int64)
  | VArrow of string

  (* special cases *) 
  | VNone | VSome of v
  | VList of v list
  | VRef of v

  | VTODO of string
(*e: type [[OCaml.v]] *)

(*s: signature [[OCaml.vof_xxx]] functions *)
(* building blocks, used by code generated using ocamltarzan *)
val vof_unit   : unit -> v
val vof_bool   : bool -> v
val vof_int    : int -> v
val vof_float   : float -> v
val vof_string : string -> v
val vof_list   : ('a -> v) -> 'a list -> v
val vof_option : ('a -> v) -> 'a option -> v
val vof_ref    : ('a -> v) -> 'a ref -> v
val vof_either    : ('a -> v) -> ('b -> v) -> ('a, 'b) Common.either -> v
val vof_either3    : ('a -> v) -> ('b -> v) -> ('c -> v) -> 
  ('a, 'b, 'c) Common.either3 -> v
(*e: signature [[OCaml.vof_xxx]] functions *)

(*s: signature [[OCaml.xxx_ofv]] functions *)
val int_ofv:    v -> int
val float_ofv:  v -> float
val unit_ofv: v -> unit
val string_ofv: v -> string
val list_ofv: (v -> 'a) -> v -> 'a list
val option_ofv: (v -> 'a) -> v -> 'a option
(*e: signature [[OCaml.xxx_ofv]] functions *)

(*s: signature [[OCaml.string_of_v]] *)
(* regular pretty printer (not via sexp, but using Format) *)
val string_of_v: v -> string
(*e: signature [[OCaml.string_of_v]] *)

(* sexp converters *)
(*
val sexp_of_t: t -> Sexp.t
val t_of_sexp: Sexp.t -> t
val sexp_of_v: v -> Sexp.t
val v_of_sexp: Sexp.t -> v
val string_sexp_of_t: t -> string
val t_of_string_sexp: string -> t
val string_sexp_of_v: v -> string
val v_of_string_sexp: string -> v
*)

(* json converters *)
(*
val v_of_json: Json_type.json_type -> v
val json_of_v: v -> Json_type.json_type
val save_json: Common.filename -> Json_type.json_type -> unit
val load_json: Common.filename -> Json_type.json_type
*)

(*s: signature [[OCaml.map_v]] *)
(* mapper/visitor *)
val map_v: 
  f:( k:(v -> v) -> v -> v) -> 
  v -> 
  v
(*e: signature [[OCaml.map_v]] *)

(*s: signature [[OCaml.map_of_xxx]] functions *)
(* other building blocks, used by code generated using ocamltarzan *)
val map_of_unit: unit -> unit
val map_of_bool: bool -> bool
val map_of_int: int -> int
val map_of_float: float -> float
val map_of_char: char -> char
val map_of_string: string -> string
val map_of_ref: ('a -> 'b) -> 'a ref -> 'b ref
val map_of_ref_do_nothing_share_ref: ('a -> 'a) -> 'a ref -> 'a ref
val map_of_option: ('a -> 'b) -> 'a option -> 'b option
val map_of_list: ('a -> 'a) -> 'a list -> 'a list
val map_of_either: 
  ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) Common.either -> ('b, 'd) Common.either
val map_of_either3: 
  ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> 
  ('a, 'c, 'e) Common.either3 -> ('b, 'd, 'f) Common.either3
(*e: signature [[OCaml.map_of_xxx]] functions *)

(*s: signature [[OCaml.v_xxx]] functions *)
(* pure visitor building blocks, used by code generated using ocamltarzan *)
val v_unit: unit -> unit
val v_bool: bool -> unit
val v_int: int -> unit
val v_float: float -> unit
val v_string: string -> unit
val v_option: ('a -> unit) -> 'a option -> unit
val v_list: ('a -> unit) -> 'a list -> unit
val v_ref_do_visit: ('a -> unit) -> 'a ref -> unit
val v_ref_do_not_visit: ('a -> unit) -> 'a ref -> unit
val v_either: 
  ('a -> unit) -> ('b -> unit) -> 
  ('a, 'b) Common.either -> unit
val v_either3: 
  ('a -> unit) -> ('b -> unit) -> ('c -> unit) ->
  ('a, 'b, 'c) Common.either3 -> unit
(*e: signature [[OCaml.v_xxx]] functions *)
(*e: pfff/commons/OCaml.mli *)
