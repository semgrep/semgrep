open Utils_ruby

open Il_ruby

type pos2 = Parse_info.t

val mkstmt : stmt_node -> pos2 -> stmt

val update_stmt : stmt -> stmt_node -> stmt


val fold_stmt : ('a -> stmt -> 'a) -> 'a -> stmt -> 'a
val compute_cfg : stmt -> unit


val empty_stmt : unit -> stmt

val fresh_local : stmt -> identifier

val pos_of : stmt -> pos2

val msg_id_of_string : string -> msg_id

class type cfg_visitor = object

  method visit_stmt : stmt Visitor.visit_method

  method visit_id : identifier Visitor.visit_method
  method visit_literal : literal Visitor.visit_method
  method visit_expr : expr Visitor.visit_method
  method visit_lhs : lhs Visitor.visit_method
  method visit_tuple : tuple_expr Visitor.visit_method
  method visit_rescue_guard : rescue_guard Visitor.visit_method
  method visit_def_name : def_name Visitor.visit_method
  method visit_class_kind : class_kind Visitor.visit_method
  method visit_method_param : method_formal_param Visitor.visit_method
  method visit_msg_id : msg_id Visitor.visit_method
  method visit_block_param : block_formal_param Visitor.visit_method
end

(* A visitor that walks every node in a CFG *)
class default_visitor : cfg_visitor

(* A visitor that walks every node in the current scope.  For
   instance, nested method bodies are not visited, however method
   blocks are. *)
class scoped_visitor : cfg_visitor

val visit_stmt : cfg_visitor -> stmt -> stmt

val visit_msg_id : cfg_visitor -> msg_id -> msg_id
val visit_id : cfg_visitor -> identifier -> identifier
val visit_block_param : cfg_visitor -> block_formal_param -> block_formal_param

val visit_literal : cfg_visitor ->literal -> literal
val visit_expr : cfg_visitor ->expr -> expr
val visit_lhs : cfg_visitor -> lhs -> lhs
val visit_star_expr : cfg_visitor -> star_expr -> star_expr
val visit_tuple : cfg_visitor -> tuple_expr -> tuple_expr
val visit_rescue_guard : cfg_visitor -> rescue_guard -> rescue_guard
val visit_class_kind : cfg_visitor -> class_kind -> class_kind
val visit_def_name : cfg_visitor -> def_name -> def_name
val visit_method_param : cfg_visitor -> method_formal_param -> method_formal_param

(* Converts all instances of the local variable [var] to [sub] in the
   current scope of s. *)
val alpha_convert_local : var:string -> sub:string -> stmt -> stmt

val compute_cfg_locals : ?env:StrSet.t -> stmt -> unit

(* A convenience module for easier construction of stmt nodes.  All
   expression forms include an explicit row variable to prevent
   unnecessary coercions, optional forms are presented as optional
   arguments, and common use cases have a separate, simplified
   signature.
*)
module Abbr : sig

  val mcall : ?lhs:lhs -> ?targ:expr ->
    msg_id -> star_expr list -> ?cb:codeblock -> unit -> stmt

  val seq : stmt list -> pos2 -> stmt

  val exnblock : stmt -> rescue_block list -> ?eelse:stmt -> ?ensure:stmt
    -> pos2 -> stmt

  val yield : ?lhs:lhs -> ?args:star_expr list-> pos2 -> stmt

  val assign : lhs -> tuple_expr -> pos2 -> stmt

  val if_s : expr -> t:stmt -> f:stmt -> pos2 -> stmt

  val alias_g : link:builtin_or_global -> orig:builtin_or_global -> pos2 -> stmt
  val alias_m : link:msg_id -> orig:msg_id -> pos2 -> stmt

  val return : ?v:tuple_expr -> pos2 -> stmt

  val module_s : ?lhs:lhs -> identifier -> stmt -> pos2 -> stmt

  val local : string -> identifier

  val case : ?default:stmt -> expr -> (tuple_expr*stmt) list -> pos2 -> stmt

  val nameclass : ?lhs:lhs -> identifier -> ?inh:identifier -> stmt
    -> pos2 -> stmt
  val metaclass : ?lhs:lhs -> identifier -> stmt -> pos2 -> stmt

  val meth : ?targ:identifier -> def_name -> method_formal_param list
    -> stmt -> pos2 -> stmt

  val break : ?v:tuple_expr -> pos2 -> stmt
  val redo : pos2 -> stmt
  val retry : pos2 -> stmt
  val next : ?v:tuple_expr -> pos2 -> stmt

  val rblock : rescue_guard list -> stmt -> rescue_block

  val undef : msg_id list -> pos2 -> stmt

  val expr : expr -> pos2 -> stmt

  val defined : identifier -> stmt -> pos2 -> stmt

  val while_s : expr -> stmt -> pos2 -> stmt

  val for_s : block_formal_param list -> expr -> stmt -> pos2 -> stmt
end
