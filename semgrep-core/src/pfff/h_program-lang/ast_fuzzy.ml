(* Yoann Padioleau
 *
 * Copyright (C) 2013 Facebook
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation, with the
 * special exception on linking described in file license.txt.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
 * license.txt for more details.
*)
open Common

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(* Sexp-like data structure to represent programs.
 *
 * When searching for or refactoring code, regexps are good enough most of
 * the time; tools such as 'grep' or 'sed' are great. But certain regexps
 * are tedious to write when one needs to handle variations in spacing,
 * the possibilty to have comments in the middle of the code you
 * are looking for, or newlines. Things are even more complicated when
 * you want to handle nested parenthesized expressions or statements. This is
 * because regexps can't count. For instance how would you
 * remove a namespace in C++? You would like to write a transformation
 * like:
 *
 *  - namespace my_namespace {
 *    ...
 *  - }
 *
 * but regexps can't do that[1].
 *
 * The alternative is then to use more precise tools such as 'sgrep'
 * or 'spatch'. But implementing sgrep/spatch in the usual way
 * for a new language, by matching AST against AST, can be really tedious.
 * The AST can be big and even if we can auto generate most of the
 * boilerplate code, this still takes quite some effort (see lang_php/matcher).
 *
 * Moreover, in my experience matching AST against AST lacks
 * flexibility sometimes. For instance many people want to use 'sgrep' to
 * find a method foo and so do "sgrep -e 'foo(...)'" but
 * because the matching is done at the AST level, 'foo(...)' is
 * parsed as a function call, not a method call, and so it will
 * not work. But people expect it to work because it works
 * with regexps. So 'sgrep' for PHP currently forces people to write this
 * pattern '$V->foo(...)'.
 * In the same way a pattern like '1' was originally matching
 * only expressions, but was not matching static constants because
 * again it was a different AST constructor. Actually many
 * of the extensions and bugfixes in sgrep_php/spatch_php in
 * the last year has been related to this lack of flexibility
 * because the AST was too precise.
 *
 * Enter Ast_fuzzy, a way to factorize most of the needs of
 * 'sgrep' and 'spatch' over different programming languages,
 * while being more flexible in some ways than having a precise AST.
 * It fills a niche between regexps and very-precise ASTs.
 *
 * In Ast_fuzzy we just want to keep the parenthesized information
 * from the code, and abstract away spacing, the main things that
 * regexps have troubles with, and then let people match over this
 * parenthesized cleaned-up tree in a flexible way.
 *
 * related:
 *  - xpath? but do programming languages need the full power of xpath?
 *    usually an AST just have 3 different kinds of nodes, Defs, Stmts,
 *    and Exprs.
 *  - spacegrep by Martin Jambon, relies on indentation in addition to
 *    bracket tokens (works great on files like YAML)
 *
 * See also lang_cpp/parsing_cpp/test_parsing_cpp and its parse_cpp_fuzzy()
 * and dump_cpp_fuzzy() functions. Most of the code related to Ast_fuzzy
 * is in matcher/ and called from 'sgrep' and 'spatch'.
 * For 'sgrep' and 'spatch' examples, see unit_matcher.ml as well as
 * tests/cpp/sgrep/ and tests/cpp/spatch/
 *
 * notes:
 *  [1] Actually Perl regexps are more powerful so one can do for instance:
 *  echo 'something< namespace<x<y<z,t>>>, other >' |
 *    perl -pe 's/namespace(<(?:[^<>]|(?1))*>)/foo/'
 *  => 'something< foo, other >'
 *  but it's arguably more complicated than the proposed spatch above.
 *
 * todo:
 *  - handle infix operators: parse them not as a sequence
 *    but as a tree as we want for instance '$X->foo()' to match
 *    whole expression like 'this->bar()->foo()', or we want
 *    '$X' to match '1+1' (and not only in Parens context)
 *    updated: or use ast_generic.ml for that?
 *  - same for function calls? so maybe we need to transform our
 *    original program in a lisp like AST where things are more uniform
 *    updated: or use ast_generic.ml for that?
 *  - how to handle isomorphisms like 'order of attributes don't matter'
 *    as in XHP? or class that can be mentioned anywhere in the arguments
 *    to implements? or how can we make 'class X { ... }' to also match
 *    'class X extends whatever { ... }'? or have public/static to
 *    be optional?
 *    Use regexp over trees? Use isomorphisms file as in coccinelle?
 *    Have special mark about optional things in ast_fuzzy?
 *    Derives such information from the grammar?
 *  - want powerful queries like
 *      'class X { ... function(...) { ... foo() ... } ... }
 *    so sgrep powerful for microlevel queries, and prolog for macrolevel
 *    queries. Xpath? Css selector?
*)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

type tok = Parse_info.t
type 'a wrap = 'a * tok

type tree =
  (* todo: comma *)
  | Parens of tok * (trees, tok (* comma*)) Common.either list * tok
  | Braces of tok * trees * tok
  | Angle  of tok * trees * tok
  | Bracket  of tok * trees * tok

  (* note that gcc allows $ in identifiers, so using $ for metavariables
   * means we will not be able to match such identifiers. No big deal.
  *)
  | Metavar of string wrap
  (* note that "..." are allowed in many languages, so using "..."
   * to represent a list of anything means we will not be able to
   * match specifically "...".
  *)
  | Dots of tok

  (* Should we split Tok in
   * | Kwd of string wrap
   * | Id of string wrap
   * | OtherTok of string wrap
   * ?
   * Maybe, but at least you can build a hash from the Parse_info.t to
   * the origin token if you need a better classification of Tok.
  *)
  | Tok of string wrap

and trees = tree list
(* with tarzan *)

let is_metavar s =
  s =~ "^\\$.*"
