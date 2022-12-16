open Common

(* because Str of ocaml sux with word boundary, newline, etc.
 * ex: \\bcia_pci_tbi_try2\\b
*)

let find_offset_all_matches ~regexp str =

  let substrings = Pcre.exec ~rex:regexp str in
  let res = ref [] in

  let rec loop substrings =
    let n = Pcre.num_of_subs substrings in
    for i = 1 to n -1 do
      let (pos1, pos2) = Pcre.get_substring_ofs substrings i in
      Common.push (pos1,pos2) res;
    done;
    (try
       let subs2 = Pcre.next_match ~rex:regexp substrings in
       loop subs2
     with  Not_found -> ()
    );
  in
  loop substrings;
  List.rev !res




let test_regexp () =
  let str_ast =
    "void
cia_pci_tbi_try2(struct pci_controller *hose,);
cia_pci_tbi_try2(struct pci_controller *hose,);
" in
  let s = "cia_pci_tbi_try2" in

  let re1 = Str.regexp ("\\b" ^ s ^ "\\b") in
  (try
     let _i = Str.search_forward  re1 str_ast 0 in
     pr2 "found Str";
   with Not_found ->
     pr2 "no match Str here";
  );

  let re2 = Pcre.regexp ("\\b" ^ s ^ "\\b") in
  if Pcre.pmatch ~rex:re2 str_ast
  then pr2 "found Pcre"
  else pr2 "no match Pcre here"
  ;
  let substrings = Pcre.exec ~rex:re2 str_ast in
  let rec loop substrings =
    let n = Pcre.num_of_subs substrings in
    pr2_gen (n);
    for i = 1 to n -1 do
      let (pos1, pos2) = Pcre.get_substring_ofs substrings i in
      pr2_gen (pos1,pos2);
    done;
    (try
       let subs2 = Pcre.next_match ~rex:re2 substrings in
       loop subs2
     with  Not_found -> ()
    );
  in
  loop substrings;


  pr2_gen (find_offset_all_matches re2 str_ast);

  let re3 = ("\\b" ^ s ^ "\\b") in
  let tmpfile = "/tmp/for_egrep" in
  write_file ~file:tmpfile str_ast;
  let com = spf "egrep -q '(%s)' %s" re3 tmpfile in
  (match Sys.command com with
   | 0 (* success *) -> pr2 "found egrep"
   | _ (* failure *) -> pr2 "no match egrep here"
  );
  ()


let actions () = [
  "-test_regexp", " ",
  Common.mk_action_0_arg test_regexp;
]
