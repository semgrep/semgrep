(* used to be called common_extra.ml *)

(*
 * How to use it ? ex in LFS:
 *  Console.progress (w.prop_iprop#length) (fun k ->
 *  w.prop_iprop#iter (fun (p, ip) ->
 *     k ();
 *     ...
 *  ));
 *
 * todo: Unix.isatty, and the spinner trick of jason \ | / -
 *)

let execute_and_show_progress ~show len f =
  let _count = ref 0 in
  (* kind of continuation passed to f *)
  let continue_pourcentage () =
    incr _count;
    ANSITerminal.set_cursor 1 (-1);
    ANSITerminal.printf [] "%d / %d" !_count len; flush stdout;
  in
  let nothing () = () in

  (* ANSITerminal.printf [] "0 / %d" len; flush stdout; *)
  (if !Common2._batch_mode || not show
   then f nothing
   else f continue_pourcentage
  );
  Common.pr2 ""


let execute_and_show_progress2 ?(show=true) len f =
  let _count = ref 0 in
  (* kind of continuation passed to f *)
  let continue_pourcentage () =
    incr _count;
    ANSITerminal.set_cursor 1 (-1);
    ANSITerminal.eprintf [] "%d / %d" !_count len; flush stderr;
  in
  let nothing () = () in

  (* ANSITerminal.printf [] "0 / %d" len; flush stdout; *)
  if !Common2._batch_mode || not show
  then f nothing
  else f continue_pourcentage

let with_progress_list_metter ?show fk xs =
  let len = List.length xs in
  execute_and_show_progress2 ?show len
    (fun k -> fk k xs)

let progress ?show fk xs =
  with_progress_list_metter ?show fk xs


(*
(* old code
let ansi_terminal = ref true *)

let (_execute_and_show_progress_func:
   (show:bool ->
    int (* length *) -> ((unit -> unit) -> 'a) -> 'a) ref)
 = ref
  (fun ~show a b ->
    failwith "no execute  yet, have you included common_extra.cmo?"
  )

let execute_and_show_progress ?(show=true) len f =
    !_execute_and_show_progress_func ~show len f

(* don't forget to call Common_extra.set_link () *)

val _execute_and_show_progress_func :
  (show:bool -> int (* length *) -> ((unit -> unit) -> unit) -> unit)
  ref
val execute_and_show_progress :
 ?show:bool -> int (* length *) -> ((unit -> unit) -> unit) -> unit

let set_link () =
  Common2._execute_and_show_progress_func := execute_and_show_progress


let _init_execute =
  set_link ()


*)

(* now in common_extra.ml:
 * let execute_and_show_progress len f = ...
*)
