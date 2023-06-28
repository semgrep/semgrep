type t = (string, value) Hashtbl.t
and value = Start of float | Recorded of float

let make () = Hashtbl.create 0x100

let save profiler ~name =
  match Hashtbl.find_opt profiler name with
  | Some (Start start_time) ->
      let now = Unix.gettimeofday () in
      Hashtbl.replace profiler name (Recorded (now -. start_time))
  | Some (Recorded _) -> invalid_arg "%s was already profiled"
  | None ->
      let now = Unix.gettimeofday () in
      Hashtbl.add profiler name (Start now)

let record profiler ~name fn =
  let t0 = Unix.gettimeofday () in
  let finally () =
    let t1 = Unix.gettimeofday () in
    Hashtbl.add profiler name (Recorded (t1 -. t0))
  in
  Fun.protect ~finally fn

let dump profiler =
  Hashtbl.fold
    (fun name value acc ->
      match value with
      | Recorded time -> (name, time) :: acc
      | _ -> acc)
    profiler []
