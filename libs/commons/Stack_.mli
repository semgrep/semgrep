type 'a t = 'a list ref

val push : 'a -> 'a t -> unit

(* may raise exn when the stack is empty *)
val pop : 'a t -> 'a
