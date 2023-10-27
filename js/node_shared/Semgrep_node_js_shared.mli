module Io : RPC_server.LSIO

val promise_of_lwt : (unit -> 'a Lwt.t) -> 'res Js_of_ocaml.Js.t

val console_report :
  'a 'b.
  Logs.src ->
  Logs.level ->
  over:(unit -> unit) ->
  (unit -> 'b) ->
  ('a, 'b) Logs.msgf ->
  'b
