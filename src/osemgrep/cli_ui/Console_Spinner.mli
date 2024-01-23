(* return whether or not we should show a spinner *)
val should_show_spinner : unit -> bool

(*
  Show a spinner while waiting for the user to sign in.
  delay_ms is the total delay across all frames, in milliseconds.
  We show each frame for 1/100th of the total delay.
*)
val show_spinner : int -> unit
val spinner_async : unit -> 'a Lwt.t
val erase_spinner : unit -> unit
