open Js_of_ocaml

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)

(* What is this folder for? We may have some common functionalities that we
   would like to share among purely the Node-specific Javascript code. For
   instance, our `unix.js` is something which should not be included into all
   of the JSCaml code (like the parsers), but just the test code and actual
   LSP code.

   To make this distinction apparent, we have `shared`, which is for things
   universal to all of the JSCaml code, and `node_shared`, for only things
   which should be distributed via Node.js.
*)

(*****************************************************************************)
(* Reporting *)
(*****************************************************************************)

(* JS specific IO for the RPC server *)
module Io = RPC_server.MakeLSIO (struct
  type input = in_channel
  type output = out_channel

  let read_line ic = input_line ic |> Lwt.return_some
  let stdin = stdin
  let stdout = stdout
  let flush () = flush stdout |> Lwt.return
  let atomic f oc = f oc

  let write _ str =
    (* nosem *)
    print_string str;
    Lwt.return ()

  let read_exactly ic n =
    let rec read_exactly acc = function
      | 0 -> String.of_seq (acc |> List.rev |> List.to_seq)
      | n -> read_exactly (input_char ic :: acc) (n - 1)
    in
    let exact = read_exactly [] n in
    Lwt.return (Some exact)
end)

let ppf, flush =
  let b = Buffer.create 255 in
  let flush () =
    let s = Buffer.contents b in
    Buffer.clear b;
    s
  in
  (Format.formatter_of_buffer b, flush)

let console_report _src _level ~over k msgf =
  let k _ =
    Firebug.console##error (Js.string (flush ()));
    over ();
    k ()
  in
  msgf @@ fun ?header ?tags fmt ->
  ignore tags;
  match header with
  | None -> Format.kfprintf k ppf ("@[" ^^ fmt ^^ "@]@.")
  | Some h -> Format.kfprintf k ppf ("[%s] @[" ^^ fmt ^^ "@]@.") h

(*****************************************************************************)
(* Promises *)
(*****************************************************************************)

let _Promise = Js.Unsafe.global##._Promise

let promise_of_lwt lwt =
  new%js _Promise
    (Js.wrap_callback (fun resolve reject ->
         try%lwt
           let%lwt res = lwt () in
           Js.Unsafe.fun_call resolve [| Js.Unsafe.inject res |]
         with
         | e ->
             let msg = Printexc.to_string e in
             Firebug.console##error (Js.string msg);
             Js.Unsafe.fun_call reject
               [| Js.Unsafe.inject (new%js Js.error_constr (Js.string msg)) |]))
