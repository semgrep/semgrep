let parmap _caps ~ncores ~chunksize f xs =
  Parmap.parmap ~ncores ~chunksize f (Parmap.L xs)

(* this is just because we forget every call to Parmap.$F in
 * TCB/forbid_process.jsonnet so we need that
 *)
let disable_core_pinning = Parmap.disable_core_pinning

let get_cpu_count () : int =
  (* Parmap subtracts 1 from the number of detected cores.
     This comes with no guarantees. *)
  max 1 (Parmap.get_default_ncores () + 1)
