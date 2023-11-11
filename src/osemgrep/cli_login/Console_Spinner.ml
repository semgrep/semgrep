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

let erase_spinner () : unit =
  ANSITerminal.move_cursor 0 (-1);
  ANSITerminal.move_bol ();
  ANSITerminal.erase ANSITerminal.Below
