(*****************************************************************************)
(* Console Experience *)
(*****************************************************************************)

let spinner = [| "⠋"; "⠙"; "⠹"; "⠸"; "⠼"; "⠴"; "⠦"; "⠧"; "⠇"; "⠏" |]

let show_spinner delay_ms : unit =
  let print_frame ~frame_index:i : unit =
    let spinner = spinner.(i mod Array.length spinner) in
    ANSITerminal.set_cursor 1 (-1);
    ANSITerminal.printf [ ANSITerminal.green ] "%s Waiting for sign in..."
      spinner
  in
  for frame_index = 1 to 100 do
    print_frame ~frame_index;
    (* Note: sleep is measured in seconds *)
    Unix.sleepf (Float.of_int delay_ms /. Float.of_int (1000 * 100))
  done

let lwt_platform_sleep_impl = ref None

let lwt_platform_sleep_setup () =
  if not !Common.jsoo then lwt_platform_sleep_impl := Some Lwt_unix.sleep

let lwt_platform_sleep a =
  match !lwt_platform_sleep_impl with
  | Some f -> f a
  | None -> Lwt.return_unit

let lwt_pause () = if !Common.jsoo then Lwt.return_unit else Lwt.pause ()

let erase_spinner () : unit =
  ANSITerminal.set_cursor 1 (-1);
  ANSITerminal.move_bol ();
  ANSITerminal.erase ANSITerminal.Below

let spinner_async () : 'a Lwt.t =
  (* nosemgrep *)
  ANSITerminal.(print_string [] "\027[?25l");
  (* hide cursor to make progess indicator more visible *)
  let jump_y = ref true in
  let print_frame ~frame_index:i : unit Lwt.t =
    let spinner = spinner.(i mod Array.length spinner) in
    ANSITerminal.move_bol ();
    (* ensure we update only the progress indicator *)
    if !jump_y then (
      (* jump to the line above to add the indicator *)
      ANSITerminal.move_cursor 0 (-1);
      jump_y := false)
    else ();
    ANSITerminal.printf [ ANSITerminal.green ] "%s" spinner;
    let inner_sleep = lwt_platform_sleep 0.005 in
    let%lwt () = inner_sleep in
    Lwt.return_unit
  in
  (* create a cancellable promise *)
  let rec loop i =
    let%lwt _ = print_frame ~frame_index:i in
    let%lwt _ = lwt_platform_sleep 0.025 in
    let%lwt _ = lwt_pause () in
    loop (i + 1)
  in
  Lwt.finalize
    (fun () -> loop 0)
    (fun () ->
      (* Flush any pending output to prevent surprises *)
      let%lwt () = Lwt_io.flush Lwt_io.stdout in
      (* nosemgrep *)
      ANSITerminal.(print_string [] "\027[?25h");
      erase_spinner ();
      Lwt.return_unit)
