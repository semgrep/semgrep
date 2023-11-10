(* This is the same visitor type that is used by CIL *)
type 'a visitAction =
  | SkipChildren
      (** Do not visit the children. Return
                                            the node as it is. *)
  | DoChildren
      (** Continue with the children of this
                                            node. Rebuild the node on return
                                            if any of the children changes
                                            (use == test) *)
  | ChangeTo of 'a
      (** Replace the expression with the
                                            given one *)
  | ChangeDoChildrenPost of 'a * ('a -> 'a)
      (** First consider that the entire
                                                exp is replaced by the first
                                                parameter. Then continue with
                                                the children. On return rebuild
                                                the node if any of the children
                                                has changed and then apply the
                                                function on the node *)

type 'a visit_method = 'a -> 'a visitAction

val visit : 'a visit_method -> 'a -> ('a -> 'a) -> 'a
(** [visit meth t child_f] Applies the visitor method [meth] to [t] and
    returns a possibly modified value.  If the action requires the
    children to be visited, then [child_f] is called to perform this.
*)

class type ['a] std_visitor = object
  method visit : 'a visit_method
end
