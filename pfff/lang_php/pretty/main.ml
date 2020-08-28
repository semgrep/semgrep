(*
 * The author disclaims copyright to this source code.  In place of
 * a legal notice, here is a blessing:
 *
 *    May you do good and not evil.
 *    May you find forgiveness for yourself and forgive others.
 *    May you share freely, never taking more than you give.
 *)

(*****************************************************************************)
(* Purpose *)
(*****************************************************************************)

(* Main entry point for the pretty printer *)
(* Takes in php files, spits out their pretty printed version on stdout *)

(*****************************************************************************)
(* Flags *)
(*****************************************************************************)

let verbose = ref false

(* action mode *)
let action = ref ""

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let make_tmp() =
  let fn = Filename.temp_file "" ".php" in
  let oc = open_out fn in
  try
    while true do
      output_string oc (read_line());
      output_string oc "\n"
    done;
    assert false
  with End_of_file ->
    close_out oc;
    fn

let print_token tok =
  let inf = Token_helpers_php.info_of_tok tok in
  let tok = Parse_info.str_of_info inf in
  let line = Parse_info.line_of_info inf in
  Printf.printf "/BEGIN[%d]:%s:END/\n" line tok

(*****************************************************************************)
(* Main action *)
(*****************************************************************************)

let main_action files = 
  let files = if files = [] then [make_tmp()] else files in
  List.iter (
  fun file ->
    let tokens = Parse_php.tokens file in
(*     List.iter print_token tokens; if true then exit 0; *)
    let ast    = Parse_php.parse_program file in
    let ast    = Ast_pp_build.program_with_comments tokens ast in
    Pretty_print.program print_string ast
 ) files


(*****************************************************************************)
(* The options *)
(*****************************************************************************)

let all_actions () = 
 []

let options () = 
  [
    "-verbose", Arg.Set verbose, 
    " ";
    "-indent", Arg.Set_int Pretty_print_code.margin_offset, 
    " <int>";
  ] @
  Common.options_of_actions action (all_actions())



(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

let main () = 
  let usage_msg = 
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ 
      " [options] <files> " ^ "\n" ^ "Options are:"
  in
  (* does side effect on many global flags *)
  let args = Common.parse_options (options()) usage_msg Sys.argv in

  (* must be done after Arg.parse, because Common.profile is set by it *)
  Common.profile_code "Main total" (fun () -> 

    (match args with
   
    (* --------------------------------------------------------- *)
    (* actions, useful to debug subpart *)
    (* --------------------------------------------------------- *)
    | xs when List.mem !action (Common.action_list (all_actions())) -> 
        Common.do_action !action xs (all_actions())

    | _ when not (Common.null_string !action) -> 
        failwith ("unrecognized action or wrong params: " ^ !action)

    (* --------------------------------------------------------- *)
    (* main entry *)
    (* --------------------------------------------------------- *)
    | x::xs -> 
        main_action (x::xs)

    (* --------------------------------------------------------- *)
    (* empty entry *)
    (* --------------------------------------------------------- *)
    | [] -> 
        Common.usage usage_msg (options()); 
        failwith "too few arguments"
    )
  )



(*****************************************************************************)
let _ =
  Common.main_boilerplate (fun () -> 
      main ();
  )
