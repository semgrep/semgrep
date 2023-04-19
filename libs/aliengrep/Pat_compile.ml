(*
   Compile a pattern into a regexp.

   PCRE pattern reference:
     https://www.pcre.org/original/doc/html/pcrepattern.html

   Dig into the test outputs to see what the generated PCRE code looks like
   e.g. cat ~/semgrep/_build/default/tests/_build/_tests/semgrep-core/aliengrep.014.output
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
  (*
     List of the PCRE capturing groups that we care about for extracting
     metavariable values.
     This is a mapping from a PCRE group ID to a metavariable.
     Not all capturing groups are of interest because the special '(DEFINE)'
     required for subroutine definitions introduces useless
     capturing groups.

     A pattern '$FOO $FOO' is translated into a PCRE pattern that looks
     like '... ((?&word)) (?&whitespace) \g{3}'. The definitions of
     the subtroutines 'word' and 'whitespace' were elided.
     The parentheses around '(?&word)' define the capturing group.
     \g{3} is a back-reference requiring an exact match of whatever
     the capturing group #3 captured. This example assumes that '((?&word))'
     is capturing group #3.
     In this example, 'metavariable_groups' is '[3, (Metavariable, "FOO")]'.

     Try this in utop:

#require "pcre";;

let pat = {|
(?(DEFINE) (?<word> [a-z]+))
(?(DEFINE) (?<whitespace> [[:space:]]* ))
((?&word)) (?&whitespace) \g{3}
|}
;;

let rex = Pcre.regexp ~flags:[`EXTENDED] pat in
Pcre.extract_all ~rex {|xx ab ab xx|};;
- : string array array = [|[|"ab ab"; ""; ""; "ab"|]|]

     Note that you'd get more matches if the word pattern was inlined
     (due to backtracking now taking place).
     Try it.
  *)
  metavariable_groups : (int * metavariable) list;
}
[@@deriving show]

(*
   PCRE patterns and names of PCRE patterns that depend whether we allow
   newlines as ordinary whitespace.
*)
type param = {
  (* ignorable whitespace pattern, which or may or may not include newlines *)
  whitespace_pat : string;
  (* other_char:
     - must match any single significant non-word character such as punctuation
       or non-ASCII characters.
     - may not match ignorable whitespace.
     - may match a word character but it's not required.
  *)
  other_char : string;
  bracket_name : string;
  node_name : string;
}

(* uniline mode and regular ellipsis "..." *)
let uniline_param =
  {
    whitespace_pat = {|[[:blank:]]*+|};
    other_char = {|[^[:space:]]|};
    bracket_name = "ubracket";
    node_name = "unode";
  }

(* multiline mode and long ellipsis "...." *)
let multiline_param =
  {
    whitespace_pat = {|[[:space:]]*+|};
    other_char = {|[^[:blank:]]|};
    bracket_name = "mbracket";
    node_name = "mnode";
  }

(* sequence of any nodes to be captured by a regular ellipsis or by a long
   ellipsis. It uses lazy quantifiers so as to favor shorter matches over
   longer matches ('?' -> '??', '*' -> '*?', '+' -> '+?').

   Warning from PCRE: "All subroutine calls, whether recursive or not,
   are always treated as atomic groups"

   Because of this limitation and because we need backtracking when matching
   an ellipsis followed by a specific node, this pattern must remain inline.
*)
let ellipsis_pat param =
  sprintf {|(?: (?&%s)(?:%s(?&%s))*? )??|} param.node_name param.whitespace_pat
    param.node_name

(* We generate a rather complex PCRE pattern. The syntax assumes the
   so-called extended mode which ignores whitespace and #-comments.

   The generated code has two parts:
   1. subroutine definitions, which assign name to patterns without
      using them. Some of these definitions are mutually recursive due
      to bracket matching. Patterns using mutually-recursive definitions
      are no longer regular expressions.
   2. the main pattern ("regexp") which references some of the
      subroutines.

   See the PCRE manual for the syntax and behavior (URL at the beginning
   of this file).
*)
let to_regexp (conf : Conf.t) (ast : Pat_AST.t) =
  let new_capturing_group =
    let n = ref 0 in
    (* start numbering groups from 1 *)
    fun () ->
      incr n;
      !n
  in
  let capturing_groups = ref [] in
  let get_capturing_group_array () = List.rev !capturing_groups in
  (*
     Create a subroutine i.e. a named PCRE pattern. The name must be a
     valid identifier for PCRE (alphanumeric or something).

     Warning: the special '(DEFINE)' sequence creates a capturing group
     that never matches anything but we have to take it into account
     in the numbering of capturing groups when extracting metavariable
     captures.
  *)
  let define name pat =
    new_capturing_group () |> ignore;
    sprintf {|(?(DEFINE)(?<%s> %s))|} name pat
  in
  let default_param =
    if conf.multiline then multiline_param else uniline_param
  in
  (* sequence of whitespace characters *)
  let def_ws = define "ws" default_param.whitespace_pat in
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
             sprintf {|%s%s%s|}
               (String.make 1 open_ |> Pcre_util.quote)
               (ellipsis_pat param)
               (String.make 1 close |> Pcre_util.quote))
      |> String.concat " | ")
  in
  let def_node param =
    define param.node_name
      (sprintf {|(?&word)|(?&%s)|%s|} param.bracket_name param.other_char)
  in
  let parametrized_definitions param = [ def_bracket param; def_node param ] in
  let definitions =
    [ def_lwb; def_rwb; def_ws; def_word ]
    @ parametrized_definitions uniline_param
    @ parametrized_definitions multiline_param
  in
  let word str =
    (* match a specific word *)
    sprintf {|(?&lwb)%s(?&rwb)|} (Pcre_util.quote str)
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
      | Ellipsis when not conf.multiline -> add (ellipsis_pat uniline_param)
      | Long_ellipsis
      | Ellipsis ->
          add (ellipsis_pat multiline_param)
      | Metavar name -> add (capture (Metavariable, name) {|(?&word)|})
      | Metavar_ellipsis name when not conf.multiline ->
          add
            (capture (Metavariable_ellipsis, name) (ellipsis_pat uniline_param))
      | Long_metavar_ellipsis name
      | Metavar_ellipsis name ->
          add
            (capture
               (Metavariable_ellipsis, name)
               (ellipsis_pat multiline_param))
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
  let root =
    sprintf {|# definitions
%s

# entry point
%s
|}
      (String.concat "\n" definitions)
      entrypoint
  in
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
