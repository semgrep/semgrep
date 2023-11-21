(* note that Parmap does not work under Windows, so do not put
 * this file under commons/ as we want commons/ to remain
 * portable
 *)

let get_cpu_count () : int =
  (* Parmap subtracts 1 from the number of detected cores.
     This comes with no guarantees. *)
  max 1 (Parmap.get_default_ncores () + 1)
