(* Functions used to instrument the code *)

let with_span = Trace_core.with_span

(* Setting up the backend *)

let with_setup f = Trace_tef.with_setup ~out:(`File "trace.json") () @@ f
