(* Ordinary bodies must be lowered even without a surrounding let binding. *)
let direct () =
  (* ruleid: expression-statements *)
  sink (source ())

let sequence () =
  let x = source () in
  ignore 0;
  (* ruleid: expression-statements *)
  sink x

(* The final expression of a sequence supplies its value. *)
let sequence_value () =
  let x = (ignore 0; source ()) in
  (* ruleid: expression-statements *)
  sink x

let clean_sequence_value () =
  let x = (ignore (source ()); 0) in
  (* ok: expression-statements *)
  sink x

let clean () =
  (* ok: expression-statements *)
  sink 0

(* A write in one case must not flow through the body of a different case. *)
let match_no_fallthrough tag =
  let cell = { value = 0 } in
  (match tag with
   | 0 -> cell.value <- source ()
   | 1 ->
       (* ok: expression-statements *)
       sink cell.value
   | _ ->
       (* ok: expression-statements *)
       sink cell.value);
  (* A write in any case can reach the continuation after the match. *)
  (* ruleid: expression-statements *)
  sink cell.value
