open Common

module Flag = Flag_parsing

(*****************************************************************************)
(* Subsystem testing *)
(*****************************************************************************)

let test_tokens_sql file =
  if not (file =~ ".*\\.sql")
  then pr2 "warning: seems not a .sql file";

  Flag.verbose_lexing := true;
  Flag.verbose_parsing := true;

  let toks = Parse_sql.tokens file in
  toks |> List.iter (fun x -> pr2_gen x);
  ()

(* ------------------------------------------------------------------------ *)
let test_parse_sql file  =
  let _x = Parse_sql.parse file in
  ()


(* ------------------------------------------------------------------------ *)
let stress_parse_sql file =
  let xs = Common.cat file in

  let ys = Common2.split_list_regexp "^-----------" xs in
  let bad = ref 0 in
  let nbtotal = List.length ys in

  ys |> List.iter (fun (_heading, xs) ->
    let s = Common.join " " xs in
    try
      let _ =
        Parse_sql.parse_string s
      in
      ()
    with exn ->
      pr2 (spf "PARSING PB, exn = %s, sql = %s" (Common.exn_to_s exn) s);
      incr bad;
  );
  pr2 (spf "nb failures = %d / %d" !bad nbtotal);
  ()

(*****************************************************************************)
(* Main entry for Arg *)
(*****************************************************************************)
let actions () = [
  "-lexer_sql", "   <file>",
  Common.mk_action_1_arg test_tokens_sql;
  "-parse_sql", "   <file>",
  Common.mk_action_1_arg test_parse_sql;
  "-stress_sql", "   <file>",
  Common.mk_action_1_arg stress_parse_sql;
]
