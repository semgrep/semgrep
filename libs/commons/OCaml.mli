(*
 * OCaml hacks to support reflection (works with ocamltarzan and tools/otarzan).
 *
 * See also sexp.ml, JSON.ml, and xml.ml in pfff for other "reflective"
 * techniques.
 *
 * DEPRECATED: this module is deprecated, you should prefer to use
 * the deriving ppx technique (e.g., deriving show).
 *)

(* OCaml values (a restricted form of expressions) *)
type v =
  | VUnit
  | VBool of bool
  | VFloat of float
  | VInt of int64
  | VChar of char
  | VString of string
  | VTuple of v list
  | VDict of (string * v) list
  | VSum of string * v list
  | VVar of (string * int64)
  | VArrow of string
  (* special cases *)
  | VNone
  | VSome of v
  | VList of v list
  | VRef of v
  | VTODO of string

(*
   Regular pretty printer (not via sexp, but using Format).

   Use max_depth=1 to show only the root construct, use max_depth=2 to
   show two levels deep, etc. The default is to show the whole tree.
*)
val string_of_v : ?max_depth:int -> v -> string

(* mapper/visitor *)
val map_v : f:(k:(v -> v) -> v -> v) -> v -> v

(* building blocks, used by code generated using ocamltarzan *)
val vof_unit : unit -> v
val vof_bool : bool -> v
val vof_int : int -> v
val vof_int64 : int64 -> v
val vof_float : float -> v
val vof_string : string -> v
val vof_list : ('a -> v) -> 'a list -> v
val vof_option : ('a -> v) -> 'a option -> v
val vof_ref : ('a -> v) -> 'a ref -> v
val vof_either : ('a -> v) -> ('b -> v) -> ('a, 'b) Either.t -> v

val vof_either3 :
  ('a -> v) -> ('b -> v) -> ('c -> v) -> ('a, 'b, 'c) Either_.either3 -> v

val vof_all3 : ('a -> v) -> ('b -> v) -> ('c -> v) -> 'a * 'b * 'c -> v
val int_ofv : v -> int64
val float_ofv : v -> float
val unit_ofv : v -> unit
val string_ofv : v -> string
val list_ofv : (v -> 'a) -> v -> 'a list
val option_ofv : (v -> 'a) -> v -> 'a option

(* TODO? Move to a Otarzan_runtime.ml ? *)
(* other building blocks, used by code generated using ocamltarzan *)
val map_of_unit : unit -> unit
val map_of_bool : bool -> bool
val map_of_int : int -> int
val map_of_int64 : int64 -> int64
val map_of_float : float -> float
val map_of_char : char -> char
val map_of_string : string -> string
val map_of_ref : ('a -> 'b) -> 'a ref -> 'b ref
val map_of_ref_do_nothing_share_ref : ('a -> 'a) -> 'a ref -> 'a ref
val map_of_option : ('a -> 'b) -> 'a option -> 'b option
val map_of_list : ('a -> 'b) -> 'a list -> 'b list

val map_of_either :
  ('a -> 'b) -> ('c -> 'd) -> ('a, 'c) Either.t -> ('b, 'd) Either.t

val map_of_either3 :
  ('a -> 'b) ->
  ('c -> 'd) ->
  ('e -> 'f) ->
  ('a, 'c, 'e) Either_.either3 ->
  ('b, 'd, 'f) Either_.either3

val map_of_all3 :
  ('a -> 'b) -> ('c -> 'd) -> ('e -> 'f) -> 'a * 'c * 'e -> 'b * 'd * 'f

(* pure visitor building blocks, used by code generated using ocamltarzan *)
val v_unit : unit -> unit
val v_bool : bool -> unit
val v_int : int -> unit
val v_float : float -> unit
val v_string : string -> unit
val v_option : ('a -> unit) -> 'a option -> unit
val v_list : ('a -> unit) -> 'a list -> unit
val v_ref_do_visit : ('a -> unit) -> 'a ref -> unit
val v_ref_do_not_visit : ('a -> unit) -> 'a ref -> unit
val v_either : ('a -> unit) -> ('b -> unit) -> ('a, 'b) Either.t -> unit

val v_either3 :
  ('a -> unit) ->
  ('b -> unit) ->
  ('c -> unit) ->
  ('a, 'b, 'c) Either_.either3 ->
  unit

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
val save_json: string (* filename *) -> Json_type.json_type -> unit
val load_json: string (* filename *) -> Json_type.json_type
*)
