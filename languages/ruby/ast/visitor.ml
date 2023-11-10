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

let visit meth orig recurse =
  match meth orig with
  | SkipChildren -> orig
  | DoChildren -> recurse orig
  | ChangeTo changed -> changed
  | ChangeDoChildrenPost (res1, f) -> f (recurse res1)

class type ['a] std_visitor = object
  method visit : 'a visit_method
end
