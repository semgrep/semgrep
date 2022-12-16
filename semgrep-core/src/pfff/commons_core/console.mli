(* to be used as in
 *  xs +> Common_extra.progress (fun k -> List.iter (fun x -> k(); ...))
*)
val progress:
  ?show:bool -> ((unit -> unit) -> 'a list -> 'b) -> 'a list -> 'b


val execute_and_show_progress:
  show:bool -> int -> ((unit -> unit) -> 'a) -> unit
val execute_and_show_progress2:
  ?show:bool -> int -> ((unit -> unit) -> 'a) -> 'a

val with_progress_list_metter:
  ?show:bool -> ((unit -> unit) -> 'a list -> 'b) -> 'a list -> 'b
