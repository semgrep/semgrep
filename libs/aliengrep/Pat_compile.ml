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
     the subroutines 'word' and 'whitespace' were elided.
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
   Parameters used to create patterns for a given definition of whitespace.
   This is used to match whitespace between pattern elements and
   within ellipses (regular or long).
*)
type spacing_param = {
  (* ignorable whitespace pattern, which or may or may not include newlines *)
  whitespace_pat : string;
  node_name : string;
  bracket_name : string;
}

(* Parameters derived from a user configuration of type Conf.t *)
type param = {
  multiline_mode : bool;
  spacing : spacing_param;
  ellipsis : spacing_param;
  long_ellipsis : spacing_param;
}

(* uniline mode and regular ellipsis "..." *)
let uniline_spacing_param =
  {
    whitespace_pat = {|[[:blank:]]*+|};
    bracket_name = "u_bracket";
    node_name = "u_node";
  }

(* multiline mode and long ellipsis "...." *)
let multiline_spacing_param =
  {
    whitespace_pat = {|[[:space:]]*+|};
    bracket_name = "m_bracket";
    node_name = "m_node";
  }

let param_of_conf (conf : Conf.t) =
  let multiline_mode = conf.multiline in
  if multiline_mode then
    {
      multiline_mode;
      spacing = multiline_spacing_param;
      ellipsis = multiline_spacing_param;
      long_ellipsis = multiline_spacing_param;
    }
  else
    {
      multiline_mode = false;
      spacing = uniline_spacing_param;
      ellipsis = uniline_spacing_param;
      long_ellipsis = multiline_spacing_param;
    }

(* sequence of any nodes to be captured by a regular ellipsis or by a long
   ellipsis. It uses lazy quantifiers so as to favor shorter matches over
   longer matches ('?' -> '??', '*' -> '*?', '+' -> '+?').

   Warning from PCRE: "All subroutine calls, whether recursive or not,
   are always treated as atomic groups"

   Because of this limitation and because we need backtracking when matching
   an ellipsis followed by a specific node, this pattern must remain inline.
*)
let ellipsis_pat_of_spacing_param ~excluded_brace sp =
  let exclude_char =
    match excluded_brace with
    | None -> ""
    | Some c -> sprintf {|(?!%s)|} (Pcre.quote (String.make 1 c))
  in
  sprintf {|(?: %s(?&%s)(?:%s%s(?&%s))*? )??|} exclude_char sp.node_name
    sp.whitespace_pat exclude_char sp.node_name

let ellipsis_pat ~excluded_brace param =
  ellipsis_pat_of_spacing_param ~excluded_brace param.ellipsis

(* In addition to allowing newlines in-between the sequence elements,
   a long ellipsis pattern must allow leading and trailing whitespace
   containing newlines in uniline mode.
   We try to include as little leading/trailing whitespace as possible,
   though.
*)
let long_ellipsis_pat ~excluded_brace param =
  if param.multiline_mode then
    (* in multiline mode, a long ellipsis is the same as a regular
       ellipsis. This minimize leading and trailing whitespace captured
       by the ellipsis. *)
    ellipsis_pat_of_spacing_param ~excluded_brace param.ellipsis
  else
    (* uniline mode *)
    sprintf {|(?:\n %s)?? %s (?:%s \n)??|} param.long_ellipsis.whitespace_pat
      (ellipsis_pat_of_spacing_param ~excluded_brace param.long_ellipsis)
      param.long_ellipsis.whitespace_pat

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
  let param = param_of_conf conf in
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
  (* sequence of whitespace characters *)
  let def_ws = define "ws" param.spacing.whitespace_pat in
  let word_char = Pcre_util.char_class_of_list conf.word_chars in
  (* match any word *)
  let def_word = define "word" (sprintf {|(?&lwb)%s++|} word_char) in
  (* left word boundary *)
  let def_lwb = define "lwb" (sprintf {|(?<!%s)|} word_char) in
  (* right word boundary *)
  let def_rwb = define "rwb" (sprintf {|(?!%s)|} word_char) in
  (* not in the middle of a word *)
  let def_not_in_word =
    define "not_in_word" (sprintf {|(?!(?<=%s)%s)|} word_char word_char)
  in
  (* other_char:
     - must match any single significant non-word character such as punctuation
       or non-ASCII characters.
     - must exclude word characters.
     - must exclude the closing brace character if one is expected.
     - may not match ignorable whitespace (so that $...X doesn't capture
       leading or trailing whitespace).
     - may not match newline characters in uniline mode (except in
       long ellipses)
  *)
  let def_other =
    let excluded_chars =
      Pcre_util.char_class_of_list ~contents_only:true conf.word_chars
    in
    let pat = sprintf {|[^[:space:]%s]|} excluded_chars in
    define "other" pat
  in
  let def_bracket sparam =
    define sparam.bracket_name (* = u_bracket or m_bracket *)
      (conf.brackets
      |> Common.map (fun (open_, close) ->
             sprintf {|%s%s%s|}
               (String.make 1 open_ |> Pcre_util.quote)
               (ellipsis_pat_of_spacing_param ~excluded_brace:(Some close)
                  sparam)
               (String.make 1 close |> Pcre_util.quote))
      |> String.concat "\n  | ")
  in
  let def_node sparam =
    define sparam.node_name
      (sprintf {|(?&word)|(?&%s)|(?&other)|} sparam.bracket_name)
  in
  let parametrized_definitions sparam =
    [ def_bracket sparam; def_node sparam ]
  in
  let definitions =
    [ def_lwb; def_rwb; def_not_in_word; def_ws; def_word; def_other ]
    @ parametrized_definitions uniline_spacing_param
    @ parametrized_definitions multiline_spacing_param
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
    | Some (backref_num, (Metavariable, _)) ->
        (* Ignore the pattern, instead require an exact occurrence of what
           the metavariable matched earlier.
           The assertions lwb and rwb (word boundaries) ensure that
           matched words are whole, and not subwords e.g. the pattern
           "$A ... $A" may not match "ab b" or "a ab".
        *)
        sprintf {|(?&lwb)\g{%d}(?&rwb)|} backref_num
    | Some (backref_num, (Metavariable_ellipsis, _)) ->
        (* Ellipses may match elements other than words, so the assertions
           at the extremities of the match are more complicated:
           - if the extremity is a word character, its outer neighbor
             may not be a word character.
           - if the extremity is not a word character, its outer neighbor
             can be anything.

           Known bug: whitespace matched by the referenced group must be
           matched exactly again. We don't want that but I don't know
           a good solution.
        *)
        sprintf {|(?&not_in_word)\g{%d}(?&not_in_word)|} backref_num
  in
  let entrypoint =
    let acc = ref [] in
    let add str = acc := str :: !acc in
    (* excluded_brace = closing brace that can only be used as a closing
       brace matching the currently open brace. This is to prevent
       a semgrep pattern like '(...x)' from matching '()x)'.
       Other closing brace characters that may be found are mismatched
       and treated as ordinary punctuation characters ("other") forming
       a node.
    *)
    let rec of_node ~excluded_brace (node : Pat_AST.node) =
      match node with
      | Ellipsis -> add (ellipsis_pat ~excluded_brace param)
      | Long_ellipsis -> add (long_ellipsis_pat ~excluded_brace param)
      | Metavar name -> add (capture (Metavariable, name) {|(?&word)|})
      | Metavar_ellipsis name ->
          add
            (capture
               (Metavariable_ellipsis, name)
               (ellipsis_pat ~excluded_brace param))
      | Long_metavar_ellipsis name ->
          add
            (capture
               (Metavariable_ellipsis, name)
               (long_ellipsis_pat ~excluded_brace param))
      | Bracket (open_, seq, close) ->
          add (Pcre_util.quote (String.make 1 open_));
          of_seq ~excluded_brace:(Some close) seq;
          add (Pcre_util.quote (String.make 1 close))
      | Word str -> add (word str)
      | Other str -> add (Pcre_util.quote str)
    and of_seq ~excluded_brace xs = List.iter (of_node ~excluded_brace) xs in
    of_seq ~excluded_brace:None ast;
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
