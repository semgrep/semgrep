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
  let blank = {|[[:blank:]]*+|} in
  let space = {|[[:space:]]*+|} in
  (* whitespace *)
  let def_ws =
    sprintf {|(?(DEFINE)(?<ws>%s))|} (if conf.multiline then space else blank)
  in
  let word_char = Pcre_util.char_class_of_list conf.word_chars in
  let def_word =
    (* match any word *)
    sprintf {|(?(DEFINE)(?<word>%s++))|} word_char
  in
  (* left word boundary *)
  let def_lwb = sprintf {|(?(DEFINE)(?<lwb>(?<!%s)))|} word_char in
  (* right word boundary *)
  let def_rwb = sprintf {|(?(DEFINE)(?<rwb>(?!%s)))|} word_char in
  let def_bracket =
    sprintf {|(?(DEFINE)(?<bracket>%s))|}
      (conf.braces
      |> Common.map (fun (open_, close) ->
             sprintf {|%s(?&seq)%s|}
               (String.make 1 open_ |> Pcre_util.quote)
               (String.make 1 close |> Pcre_util.quote))
      |> String.concat "|")
  in
  let def_node =
    (* Note that '.' may not match newlines. This is incompatible with
       the `DOTALL option of Pcre. *)
    sprintf {|(?(DEFINE)(?<node>(?&word)|(?&bracket)|(?&ws)|.))|}
  in
  (* multiline sequence of any nodes *)
  let def_mseq =
    (* lazy repeat *)
    sprintf {|(?(DEFINE)(?<mseq>%s(?:(?&node)%s)*?))|} space space
  in
  (* uniline sequence of any nodes *)
  let def_useq =
    (* lazy repeat *)
    sprintf {|(?(DEFINE)(?<useq>%s(?:(?&node)%s)*?))|} blank blank
  in
  (* sequence of nodes *)
  let def_seq =
    sprintf {|(?(DEFINE)(?<seq>(?&%s)))|}
      (if conf.multiline then "mseq" else "useq")
  in
  let definitions =
    [
      def_lwb;
      def_rwb;
      def_ws;
      def_word;
      def_bracket;
      def_node;
      def_mseq;
      def_useq;
      def_seq;
    ]
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
      | Ellipsis -> add {|(?&useq)|}
      | Long_ellipsis -> add {|(?&mseq)|}
      | Metavar name -> add (capture (Metavariable, name) {|(?&word)|})
      | Metavar_ellipsis name ->
          add (capture (Metavariable_ellipsis, name) {|(?&useq)|})
      | Long_metavar_ellipsis name ->
          add (capture (Metavariable_ellipsis, name) {|(?&mseq)|})
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
  let pcre = SPcre.regexp ~flags:[ `EXTENDED ] pcre_pattern in
  { pcre_pattern; pcre; metavariable_groups }

let from_string conf pat_str =
  Pat_parser.from_string conf pat_str |> compile conf
