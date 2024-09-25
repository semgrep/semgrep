let default_exception_handler (_x : 'a) (e : Exception.t) =
  Exception.to_string e

let parmap _caps ~ncores ~chunksize ~exception_handler f xs =
  (* Why do this? The nanny state doesn't trust you to to use parmap AND catch
     all your exceptions. And if you don't catch all your exceptions and one
     happens, then parmap will try to unmarshal your exception into the data
     type you wanted from [f], and it will fail horribly since the return type
     of your [f] is almost certainly not [exn]. So what we do here is catch all
     exceptions and return a result instead, meaning parmap will ALWAYS receive
     the correct marshaled data type *)
  let f' x =
    try Ok (f x) with
    | exn ->
        let e = Exception.catch exn in
        (* From marshal.mli in the OCaml stdlib:
         *  "Values of extensible variant types, for example exceptions (of
         *  extensible type [exn]), returned by the unmarshaller should not be
         *  pattern-matched over through [match ... with] or [try ... with],
         *  because unmarshalling does not preserve the information required for
         *  matching their constructors. Structural equalities with other
         *  extensible variant values does not work either.  Most other uses such
         *  as Printexc.to_string, will still work as expected."
         *)
        (* Because of this we cannot just catch the exception here and return
           it, as then it won't be super usable. Instead we ask the user of the
           library to handle it in the process, since then they can pattern
           match on it. They can choose to convert it to a string, a different
           datatype etc. *)
        Error (exception_handler x e)
  in
  Parmap.parmap ~ncores ~chunksize f' (Parmap.L xs)

(* this is just because we forget every call to Parmap.$F in
 * TCB/forbid_process.jsonnet so we need that
 *)
let disable_core_pinning = Parmap.disable_core_pinning

let get_cpu_count () : int =
  (* Parmap subtracts 1 from the number of detected cores.
     This comes with no guarantees. *)
  max 1 (Parmap.get_default_ncores () + 1)
