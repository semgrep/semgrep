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

let spinner_async stop : unit =
  (* nosemgrep *)
  ANSITerminal.(print_string [] "\027[?25l");
  (* hide cursor to make progess indicator more visible *)
  let jump_y = ref true in
  let print_frame ~frame_index:i : unit =
    let spinner = spinner.(i mod Array.length spinner) in
    ANSITerminal.move_bol ();
    (* ensure we update only the progress indicator *)
    if !jump_y then (
      (* jump to the line above to add the indicator *)
      ANSITerminal.move_cursor 0 (-1);
      jump_y := false)
    else ();
    (* extra guard against printing among race conditions *)
    if not !stop then ANSITerminal.printf [ ANSITerminal.green ] "%s" spinner
  in
  let rec loop () =
    if !stop then (
      (* nosemgrep *)
      ANSITerminal.(print_string [] "\027[?25h");
      (* restore cursor *)
      Lwt.return ())
    else
      let%lwt () = Lwt_unix.sleep 0.1 in
      (* create a cancellable promise *)
      for frame_index = 1 to 10 do
        print_frame ~frame_index;
        Unix.sleepf 0.05 (* ensure we have a fast refresh rate at 50ms *)
      done;
      loop ()
  in
  Lwt.async loop

let erase_spinner () : unit =
  ANSITerminal.move_cursor 0 (-1);
  ANSITerminal.move_bol ();
  ANSITerminal.erase ANSITerminal.Below
