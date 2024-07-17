open Console

let isatty () =
  !ANSITerminal.isatty UUnix.stdout && !ANSITerminal.isatty UUnix.stderr

let setup ?highlight_setting:(hs = (Auto : highlight_setting)) () =
  let hl : highlight =
    match hs with
    | Auto -> if isatty () then On else Off
    | On -> On
    | Off -> Off
  in
  highlight_setting := hs;
  highlight := hl

let with_highlight temp func =
  let orig = get_highlight_setting () in
  setup ~highlight_setting:temp ();
  Common.finalize func (fun () -> setup ~highlight_setting:orig ())

let print str = UPrintf.printf "%s\n%!" str
let print_no_nl str = UPrintf.printf "%s%!" str
let eprint str = UPrintf.eprintf "%s\n%!" str
