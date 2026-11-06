(* Calls named [returned] must be implicit returns; calls named [discarded]
   must not. Unit-valued bodies must return unit, not an earlier expression. *)

let named () = returned ()

let sequence () =
  discarded ();
  returned ()

let conditional flag = if flag then returned () else returned ()

let nested_lambda () =
  consume (fun () ->
      discarded ();
      returned ())

let unit_value () = ()

let unit_sequence () =
  discarded ();
  ()

let match_results x =
  match x with
  | 0 -> returned ()
  | 1 -> discarded (); returned ()
  | _ -> returned ()

let function_results = function
  | 0 -> returned ()
  | 1 -> returned ()
  | _ -> returned ()

let literal () = 42

let identity returned_value = returned_value
