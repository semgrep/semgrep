(* pad: this is not a great filename, but I just want to keep AST_generic.ml
 * as small as possible (it is already very big).
 *)

(*****************************************************************************)
(* Accessories *)
(*****************************************************************************)

(* A set of metavariables. Access cost is O(log n). *)
module String_set = struct
  type t = string Set_.t
  type string_list = string list [@@deriving show]

  let pp fmt x = pp_string_list fmt (Set_.elements x)
end

(*****************************************************************************)
(* Comparison and hashing *)
(*****************************************************************************)

(*
   Create a node ID that is guaranteed to be unique within an AST,
   as long as the whole AST is created within the same process.

   Such ID should not be expected to be unique across ASTs,
   since ASTs can be loaded from an external cache, for example.
*)
module Node_ID = Gensym.MkId ()

(*
   Trickery to offer two collections of equality functions:

   - structural equality: do two AST nodes have the same structure?
     This diregards IDs assigned uniquely to AST nodes.
   - referential equality: are these two AST nodes physically the same?
     This is essentially physical equality but it tolerates copying or even
     some transformation as long as the node ID is preserved.

   Comparing two AST nodes must be done via one of the with_equal_* wrappers
   so as to select structural or referential equality.
*)

type busy_with_equal = Not_busy | Structural_equal | Referential_equal

(* global state! managed by the with_equal_* functions *)
let busy_with_equal = ref Not_busy

let equal_id_info equal a b =
  match !busy_with_equal with
  | Not_busy -> failwith "Call AST_utils.with_xxx_equal to avoid this error."
  | Structural_equal -> equal a b
  | Referential_equal -> equal a b

let equal_stmt_field_s equal_stmt_kind a b =
  match !busy_with_equal with
  | Not_busy -> failwith "Call AST_utils.with_xxx_equal to avoid this error."
  | Structural_equal -> equal_stmt_kind a b
  | Referential_equal -> true

let equal_stmt_field_s_id a b =
  match !busy_with_equal with
  | Not_busy -> failwith "Call AST_utils.with_xxx_equal to avoid this error."
  | Structural_equal -> true
  | Referential_equal -> Node_ID.equal a b

(*
   Wrap one of the generated equal_* functions into one that selects
   structural equality, ignoring node IDs and position information)
*)
let with_structural_equal equal a b =
  match !busy_with_equal with
  | Not_busy ->
      busy_with_equal := Structural_equal;
      Fun.protect
        ~finally:(fun () -> busy_with_equal := Not_busy)
        (fun () -> equal a b)
  | Structural_equal
  | Referential_equal ->
      failwith "an equal is already in progress"

(*
   Wrap one of the generated equal_* functions into one that selects
   referential equality, using node IDs when available.
*)
let with_referential_equal equal a b =
  match !busy_with_equal with
  | Not_busy ->
      busy_with_equal := Referential_equal;
      Fun.protect
        ~finally:(fun () -> busy_with_equal := Not_busy)
        (fun () -> equal a b)
  | Structural_equal
  | Referential_equal ->
      failwith "an equal is already in progress"
