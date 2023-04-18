(*
   Compile a pattern into a regexp.
*)

open Printf

type metavariable_kind =
  | Metavariable
  | Metavariable_ellipsis (* regular or long *)
[@@deriving show]

(* metavariable kind, bare name *)
type metavariable = metavariable_kind * string [@@deriving show]

type t = {
  pcre_pattern : string; [@printer fun fmt -> Format.fprintf fmt "{|%s|}"]
  pcre : Pcre.regexp; [@opaque]
  (* Array of PCRE capturing groups. Each capturing group has a metavariable
     name. *)
  metavariable_groups : metavariable array;
}
[@@deriving show]

(*
   PCRE patterns and names of PCRE patterns that depend whether we allow
   newlines as ordinary whitespace.
*)
type param = {
  whitespace_seq : string;
  not_whitespace_char : string;
  bracket_name : string;
  node_name : string;
  nonempty_seq_name : string;
}

(* uniline mode and regular ellipsis "..." *)
let uniline_param =
  {
    whitespace_seq = {|[[:blank:]]*+|};
    not_whitespace_char = {|[^[:blank:]]|};
    bracket_name = "ubracket";
    node_name = "unode";
    nonempty_seq_name = "useq";
  }

(* multiline mode and long ellipsis "...." *)
let multiline_param =
  {
    whitespace_seq = {|[[:space:]]*+|};
    not_whitespace_char = {|[^[:space:]]|};
    bracket_name = "mbracket";
    node_name = "mnode";
    nonempty_seq_name = "mseq";
  }

(*
   Create a named PCRE pattern. The name must be a valid identifier
   PCRE (alphanumeric or something).
*)
let define name pat = sprintf {|(?(DEFINE)(?<%s> %s))|} name pat

(*
   to do:
   - ellipses: lazy repeat
   - metavariables: capturing groups
   - reoccurring metavariables: backreferences
   - recursive structure: reference to root pattern
*)
let to_regexp (conf : Conf.t) (ast : Pat_AST.t) =
  (* Pattern definitions

     (?(DEFINE) xxx) is a conditional pattern whose condition is always false.
     This allows us to define a named pattern without using it at the same
     time.

     (?<foo> xxx) gives the name 'foo' to the pattern xxx (and uses it).
     (?(DEFINE)(?<foo> xxx)) gives the name 'foo' to the pattern xxx without
     using it.

     (?&foo) is a reference to the pattern named foo.
  *)
  let default_param =
    if conf.multiline then multiline_param else uniline_param
  in
  (* sequence of whitespace characters *)
  let def_ws = define "ws" default_param.whitespace_seq in
  (* any non-whitespace character *)
  let def_notws = define "notws" default_param.not_whitespace_char in
  let word_char = Pcre_util.char_class_of_list conf.word_chars in
  (* match any word *)
  let def_word = define "word" (sprintf {|%s++|} word_char) in
  (* left word boundary *)
  let def_lwb = define "lwb" (sprintf {|(?<!%s)|} word_char) in
  (* right word boundary *)
  let def_rwb = define "rwb" (sprintf {|(?!%s)|} word_char) in
  let def_bracket param =
    define param.bracket_name (* = ubracket or mbracket *)
      (conf.braces
      |> Common.map (fun (open_, close) ->
             sprintf {|%s(?&%s)??%s|}
               (String.make 1 open_ |> Pcre_util.quote)
               param.nonempty_seq_name (* = useq or mseq *)
               (String.make 1 close |> Pcre_util.quote))
      |> String.concat " | ")
  in
  let def_node param =
    define param.node_name
      (sprintf {|(?&word)|(?&%s)|%s|} param.bracket_name
         param.not_whitespace_char)
  in
  (* sequence of any nodes to be captured by a regular ellipsis or a long
     ellipsis.

     Warning from PCRE: "All subroutine calls, whether recursive or not,
     are always treated as atomic groups"
     TODO: fix this by inlining any pattern that needs backtracking.
  *)
  let def_nonemptyseq param =
    (* lazy repeat: favor shortest repeat for ellipses *)
    define param.nonempty_seq_name
      (sprintf {|(?&%s)(%s(?&%s))*?|} param.node_name param.whitespace_seq
         param.node_name)
  in
  let parametrized_definitions param =
    [ def_bracket param; def_node param; def_nonemptyseq param ]
  in
  let definitions =
    [ def_lwb; def_rwb; def_ws; def_notws; def_word ]
    @ parametrized_definitions uniline_param
    @ parametrized_definitions multiline_param
  in
  let word str =
    (* match a specific word *)
    sprintf {|(?&lwb)%s(?&rwb)|} (Pcre_util.quote str)
  in
  let new_capturing_group =
    let n = ref 0 in
    (* start numbering groups from 1 *)
    fun () ->
      incr n;
      !n
  in
  let capturing_groups = ref [] in
  let get_capturing_group_array () =
    List.rev !capturing_groups |> Common.map snd |> Array.of_list
  in
  let capture metavariable pat =
    match
      List.find_opt (fun (_, mv) -> mv = metavariable) !capturing_groups
    with
    | None ->
        let num = new_capturing_group () in
        capturing_groups := (num, metavariable) :: !capturing_groups;
        sprintf {|(%s)|} pat
    | Some (backref_num, _mv) ->
        (* ignore the pattern, instead require an exact occurrence of what
           the metavariable matched earlier. *)
        sprintf {|\g{%d}|} backref_num
  in
  let entrypoint =
    let acc = ref [] in
    let add str = acc := str :: !acc in
    let rec of_node (node : Pat_AST.node) =
      match node with
      | Ellipsis -> add {|(?&useq)??|}
      | Long_ellipsis -> add {|(?&mseq)??|}
      | Metavar name -> add (capture (Metavariable, name) {|(?&word)|})
      | Metavar_ellipsis name ->
          add (capture (Metavariable_ellipsis, name) {|(?&useq)??|})
      | Long_metavar_ellipsis name ->
          add (capture (Metavariable_ellipsis, name) {|(?&mseq)??|})
      | Bracket (open_, seq, close) ->
          add (Pcre_util.quote (String.make 1 open_));
          of_seq seq;
          add (Pcre_util.quote (String.make 1 close))
      | Word str -> add (word str)
      | Other str -> add (Pcre_util.quote str)
    and of_seq xs = List.iter of_node xs in
    of_seq ast;
    let elements = List.rev !acc in
    String.concat ("\n" ^ {|(?&ws)|} ^ "\n") elements
  in
  let root = sprintf "%s\n%s" (String.concat "\n" definitions) entrypoint in
  (root, get_capturing_group_array ())

let compile conf pattern_ast =
  let pcre_pattern, metavariable_groups = to_regexp conf pattern_ast in
  (* `EXTENDED = literal whitespace and comments are ignored *)
  let pcre =
    try SPcre.regexp ~flags:[ `EXTENDED ] pcre_pattern with
    | exn ->
        (* bug *)
        let e = Exception.catch exn in
        Logs.err (fun m ->
            m "Failed to compile PCRE pattern:\n%s\n" pcre_pattern);
        Exception.reraise e
  in
  { pcre_pattern; pcre; metavariable_groups }

let from_string conf pat_str =
  Pat_parser.from_string conf pat_str |> compile conf
