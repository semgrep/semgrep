(* Yoann Padioleau
 *
 * Copyright (C) 2010-2014 Facebook
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

module E = Entity_code

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * Emacs-like font-lock-mode, or SourceInsight-like display.
 *
 * This file contains the generic part of a code highlighter
 * that is programming language independent.
 * See highlight_xxx.ml for the code specific to the 'xxx'
 * programming language.
 *
 * This source code viewer is based on good semantic information,
 * not fragile regexps (as in Emacs), or partial parsing
 * (as in SourceInsight, probably because they call cpp).
 *
 * Augmented visual! Augmented intellect! See what can not see, like in
 * movies where HUD show invisible things.
 *
 * history:
 * Some code such as the visitor code was using Emacs_mode_xxx
 * visitors before, but now we use directly the raw visitor, cos
 * emacs_mode_xxx was not a big win as we must colorize
 * and so visit and so get hooks for almost every programming constructs.
 *
 * Moreover there was some duplication, such as for the different
 * categories: I had notes in emacs_mode_xxx and also notes in this file
 * about those categories like yacfe_imprecision, cpp, etc. So
 * better and cleaner to put all related code in the same file.
 *
 *
 * Why better to have such visualisation ? cf via_pram_readbyte example:
 * - better to see that use global via1, and that global to module
 * - better to see local macro
 * - better to see that some func are local too, the via_pram_writebyte
 * - better to see if local, or parameter
 * - better to see in comments that important words such as interrupts,
 *   and 'disabled', and 'must'
 *
 *
 * SEMI do first like gtk source view
 * SEMI do first like emacs
 * SEMI do like my pad emacs mode extension
 * SEMI do for yacfe specific stuff
 *
 * less: level of font-lock-mode ? so can colorify a lot the current function
 * and less the rest (so maybe avoid some of the bugs of GText ?
 *
 * Take more ideas from Source Insight ?
 *  - variable size parens depending on depth of nestedness
 *  - do same for curly braces ?
 *
 * TODO estet: I often revisit in very similar way the code, and do
 * some matching to know if pointercall, methodcall, to know if
 * prototype or decl extern, to know if typedef inside, or structdef
 * inside. Could
 * perhaps define helpers so not redo each time same things ?
 *
 * estet?: redundant with - place_code ?  - entity_c ?
 *
 * related-work:
 *  - http://pygments.org/
 *)

(*****************************************************************************)
(* Types helpers *)
(*****************************************************************************)

(* will be italic vs non-italic (could be large vs small ? or bolder ? *)
type usedef =
  | Use
  | Def

(* colors will be adjusted (degrade de couleurs) (could also do size? *)
type place =
  | PlaceLocal
  | PlaceSameDir
  | PlaceExternal
  (* | ReallyExternal   | PlaceCloseHeader *)

  (* will be in a lighter color, almost like wheat, so know we don't have
   * information on it. Could highlight in Red because it's
   * quite similar to an error.
  *)
  | NoInfoPlace


(* will be underlined or strikedthrough *)
type def_arity =
  | UniqueDef
  | DoubleDef
  | MultiDef
  | NoDef

(* will be different colors *)
type use_arity =
  | NoUse
  | UniqueUse
  | SomeUse
  | MultiUse
  | LotsOfUse
  | HugeUse


type use_info = place * def_arity * use_arity
type def_info = use_arity
type usedef2 =
  | Use2 of use_info
  | Def2 of def_info

(*****************************************************************************)
(* Main type *)
(*****************************************************************************)

(* coupling: if you add constructor, don't forget to add its
 * handling in 2 places below, for its color and associated string
 * representation.
 *
 * If you look at usedef below, you should get all the way C programmer
 * can name things:
 *   - macro, macrovar
 *   - functions
 *   - variables (global/param/local)
 *   - typedefs, structname, enumname, enum, fields
 *   - labels
 * But at the user site, can see only if
 *  - FunCallOrMacroCall
 *  - VarOrEnumValOrMacroVar
 *  - labels
 *  - field
 *  - tag (struct, union, enum)
 *  - typedef
*)

(* color, foreground or background will be changed *)
type category =
  (* pad's addons (actually Pixel added a special font in Emacs for numbers) *)
  | Boolean | Number
  | String | Regexp
  | Atom
  | Null

  (* classic emacs mode *)
  | Keyword  (* SEMI multi *)
  | KeywordConditional
  | KeywordLoop

  | KeywordExn
  | KeywordObject
  | KeywordModule

  | KeywordConcurrency

  | Builtin
  | BuiltinCommentColor (* e.g. for "pr", "pr2", "spf". etc *)
  | BuiltinBoolean (* e.g. "not" *)

  | Operator (* TODO multi *)
  | Punctuation

  (* Functions, macros, globals, types, ... see Entity_code.entity_kind.
   * By default global scope (macro can have local
   * but not that used), so no need to like for variables and have a
   * global/local dichotomy of scope. (But even if functions are globals,
   * still can have some global/local dichotomy but at the module level.
  *)
  | Entity of Entity_code.entity_kind * usedef2

  (* kind of specific case of Global of Local which we know are really
   * really local. Don't really need a def_arity and place here. *)
  | Local of usedef
  | Parameter of usedef

  (* less: could be Entity Prototype, but there is just def for Prototype *)
  | FunctionDecl of def_info
  (* hmm does not fit Constructor use_def, because special kind of use *)
  | ConstructorMatch of use_info

  (* less: use Entity instead? *)
  | StaticMethod of usedef2
  | StructName of usedef
  | EnumName of usedef
  (* ClassName of place ... *)

  (* special types *)
  | TypeVoid | TypeInt

  (* haskell *)
  | FunctionEquation

  (* misc *)
  | Label of usedef

  (* semantic information *)

  | BadSmell
  (* less: TodoComment? *)

  (* could reuse Global (Use2 ...) but the use of refs is not always
   * the use of a global. Moreover using a ref in OCaml is really bad
   * which is why I want to highlight it specially.
  *)
  | UseOfRef

  | PointerCall (* a.k.a dynamic call *)
  | CallByRef
  | ParameterRef

  | IdentUnknown


  (* module/cpp related *)
  | Ifdef
  | Include
  | IncludeFilePath
  | Define
  | CppOther
  (* metaprogramming related *)
  | Attribute

  (* web related *)
  | EmbededCode (* e.g. javascript *)
  | EmbededUrl (* e.g. xhp *)
  | EmbededHtml (* e.g. xhp *)
  | EmbededHtmlAttr
  | EmbededStyle (* e.g. css *)
  | Verbatim (* for latex, noweb, html pre *)

  (* comments *)
  | Comment
  (* Ccomment *)
  | CommentWordImportantNotion
  | CommentWordImportantModal

  (* pad style specific *)
  | CommentSection0
  | CommentSection1
  | CommentSection2
  | CommentSection3
  | CommentSection4
  | CommentEstet
  | CommentCopyright
  | CommentSyncweb
  (* higher means more important *)
  | CommentImportance0
  | CommentImportance1
  | CommentImportance2 (* same than Comment *)
  | CommentImportance3

  (* misc *)
  | GrammarRule

  (* search and match *)
  | MatchGlimpse
  | MatchSmPL
  | MatchParent

  | MatchSmPLPositif
  | MatchSmPLNegatif


  (* basic *)
  | BackGround | ForeGround

  (* parsing imprecision *)
  | NotParsed | Passed | Expanded | Error
  | NoType

  (* well, normal code *)
  | Normal


type highlighter_preferences = {
  mutable show_type_error: bool;
  mutable show_local_global: bool;

}
let default_highlighter_preferences = {
  show_type_error = false;
  show_local_global = true;
}


(*****************************************************************************)
(* Color and font settings *)
(*****************************************************************************)

(*
 * capabilities: (cf also pango.ml)
 *  - colors, and can provide semantic information by
 *    * using opposite colors
 *    * using close colors,
 *    * using degrade color
 *    * using tone (darker, brighter)
 *    * can also use background/foreground
 *
 *  - fontsize
 *  - bold, italic, slanted, normal
 *  - underlined/strikedthrough, pango can even do double underlined
 *  - fontkind, for instance comment could be in a different font
 *    in addition of different colors ?
 *  - casse, smallcaps ? (but can confondre avec macro ?)
 *  - stretch? (condenset)
 *
 * Recurrent conventions, which would be counter productive to change maybe:
 *  - string: green
 *  - keywords: red/orange
 *
 * Emacs C-mode conventions:
 *  - entities declarations: light/dark blue
 *    (dark for param and local, light for func)
 *  - types: green
 *  - keywords: orange/dark-orange, this include:
 *    - control keywords
 *    - declaration keywords (static/register, but also struct, typedef)
 *    - cpp builtin keywords
 *  - labels: cyan
 *  - entities used: basic
 *  - comments: grey
 *  - strings: dark green
 *
 * pad:
 *  - punctuation: blue
 *  - numbers: yellow
 *
 * semantic variable:
 *   - global
 *   - parameter
 *   - local
 * semantic function:
 *   - local, defined in file
 *   - global
 *   - global and multidef
 *   - global and utilities, so kind of keyword, like my Common.map or
 *     like kprintf
 * semantic types:
 *   - local/specific
 *   - globals
 * operators:
 *   - boolean
 *   - arithmetic
 *   - bits
 *   - memory
 *
 * notions:
 *  declaration vs use (italic vs non italic, or large vs small)
 *  type vs values (use color?)
 *  control vs data (use color?)
 *  local vs global (bold vs non bold, also can use degarde de couleur)
 *  module vs program (use font size ?)
 *  unique vs multi (use underline ? and strikedthrough ?)
 *
 * more and more distant => darker ?
 * less and less unique => bigger ?
 * (but both notions of distant and unique are strongly correlated ?)
 *
 * Normally can leverage indentation and place in file. We know when
 * we are not at the toplevel because of the indentation, so can overload
 * some colors.
 *
 *
 *
 * final:
 *  (total colors)
 *  - blanc
 *      wheat: default (but what remains default??)
 *  - noir
 *      gray: comments
 *
 *
 *
 *  (primary colors)
 *  - rouge:
 *      control,  conditional vs loop vs jumps, functions
 *  - bleue:
 *     variables, values
 *  - vert:
 *       types
 *      - vert-dark: string, chars
 *
 *
 *  (secondary colors)
 *  - jaune (rouge-vert):
 *      numbers, value
 *  - magenta (rouge-bleu):
 *
 *  - cyan (vert-bleu):
 *
 *
 *  (tertiary colors)
 *  - orange (rouge-jaune):
 *
 *  - pourpre (rouge-violet)
 *
 *  - rose:
 *
 *  - turquoise:
 *
 *  - marron
 *
 *)

let legend_color_codes = "
The big principles for the colors, fonts, and strikes are:
  - italic: for definitions,
    normal: for uses
  - doubleline: double def,
    singleline: multi def,
    strike:     no def,
    normal:     single def
  - big fonts: use of global variables, or function pointer calls
  - lighter: distance of definitions (very light means in same file)

  - gray background: not parsed, no type information, or other tool limitations
  - red background:  expanded code
  - other special backgrounds: search results

  - green:  types
  - purple: fields
  - yellow: functions (and macros)
  - blue:   globals, variables
  - pink:   constants, macros

  - cyan and big:      global,
    turquoise and big: remote global,
    dark blue:         parameters,
    blue:              locals
  - yellow and big: function pointer,
    light yellow:   local call (in same file),
    dark yellow:    remote module call (in same dir)
  - salmon: many uses, probably a utility function (e.g. printf)

  - red: problem, no definitions
"


let info_of_usedef usedef =
  match usedef with
  | Def -> [`STYLE `ITALIC]
  | Use -> []

let info_of_def_arity defarity =
  match defarity with
  | UniqueDef  -> []
  | DoubleDef -> [`UNDERLINE `DOUBLE]
  | MultiDef -> [`UNDERLINE `SINGLE]

  | NoDef -> [`STRIKETHROUGH true]

let info_of_place _defplace =
  raise Todo

let info_of_entity_kind_and_usedef2 kind defkind =
  match kind, defkind with

  | E.Type, (Def2 _) -> [`FOREGROUND "chartreuse";]
  | E.Type, (Use2 _) -> [`FOREGROUND "chartreuse3";]

  | E.Constructor, (Def2 _ ) -> [`FOREGROUND "tomato1";]
  | E.Constructor, (Use2 _) -> [`FOREGROUND "pink3";]

  | E.Module, (Def2 _) -> [`FOREGROUND "chocolate";]
  | E.Module, (Use2 _) -> [`FOREGROUND "DarkSlateGray4";]

  | E.Field, (Def2 _) -> [`FOREGROUND "MediumPurple1"] @ info_of_usedef (Def)
  | E.Field, (Use2 _) -> [`FOREGROUND "MediumPurple2"] @ info_of_usedef (Use)

  | E.Exception, (Def2 _) -> [`FOREGROUND "Orchid1"] @ info_of_usedef (Def)
  | E.Exception, (Use2 _) -> [`FOREGROUND "Orchid2"] @ info_of_usedef (Use)

  (* defs *)
  | E.Function, (Def2 _) -> [`FOREGROUND "gold";
                             `WEIGHT `BOLD;`STYLE `ITALIC; `SCALE `MEDIUM;
                            ]
  | E.Macro, (Def2 _) -> [`FOREGROUND "gold";
                          `WEIGHT `BOLD;`STYLE `ITALIC; `SCALE `MEDIUM;
                         ]
  | E.Global, (Def2 _) -> [`FOREGROUND "cyan";
                           `WEIGHT `BOLD; `STYLE `ITALIC; `SCALE `MEDIUM;
                          ]
  | E.Constant, (Def2 _) -> [`FOREGROUND "pink";
                             `WEIGHT `BOLD; `STYLE `ITALIC; `SCALE `MEDIUM;
                            ]
  | E.Method, (Def2 _) -> [`FOREGROUND "gold3";
                           `WEIGHT `BOLD; `SCALE `MEDIUM;
                          ]
  (* In the end a class is really similar to a type, so better to use
   * a similar color. Anyway, at the use site we can't really know whether
   * an ident correspond to a type or a class.
  *)
  | E.Class, (Def2 _) ->  [`FOREGROUND "chartreuse2"] @ info_of_usedef (Def)

  (* uses *)
  | E.Function, (Use2 (defplace,def_arity,use_arity)) ->
      (match defplace with
       | PlaceLocal -> [`FOREGROUND "gold";]
       | PlaceSameDir -> [`FOREGROUND "goldenrod";]
       | PlaceExternal ->
           (match use_arity with
            | MultiUse -> [`FOREGROUND "DarkGoldenrod"]

            | LotsOfUse | HugeUse | SomeUse -> [`FOREGROUND "salmon";]

            | UniqueUse -> [`FOREGROUND "yellow"]
            | NoUse -> [`FOREGROUND "IndianRed";]
           )
       | NoInfoPlace -> [`FOREGROUND "LightGoldenrod";]
      ) @ info_of_def_arity def_arity

  | E.Global, (Use2 (defplace, def_arity, use_arity)) ->
      [`SCALE `X_LARGE] @
      (match defplace with
       | PlaceLocal -> [`FOREGROUND "cyan";]
       | PlaceSameDir -> [`FOREGROUND "turquoise3";]
       | PlaceExternal ->
           (match use_arity with
            | MultiUse -> [`FOREGROUND "turquoise4"]
            | LotsOfUse | HugeUse | SomeUse -> [`FOREGROUND "salmon";]

            | UniqueUse -> [`FOREGROUND "yellow"]
            | NoUse -> [`FOREGROUND "IndianRed";]
           )
       | NoInfoPlace -> [`FOREGROUND "LightCyan";]

      ) @ info_of_def_arity def_arity

  | E.Constant, (Use2 (defplace, def_arity, use_arity)) ->
      (match defplace with
       | PlaceLocal -> [`FOREGROUND "pink";]
       | PlaceSameDir -> [`FOREGROUND "LightPink";]
       | PlaceExternal ->
           (match use_arity with
            | MultiUse -> [`FOREGROUND "PaleVioletRed"]

            | LotsOfUse | HugeUse | SomeUse -> [`FOREGROUND "salmon";]

            | UniqueUse -> [`FOREGROUND "yellow"]

            | NoUse -> [`FOREGROUND "IndianRed";]
           )
       | NoInfoPlace -> [`FOREGROUND "pink1";]

      ) @ info_of_def_arity def_arity

  (* copy paste of MacroVarUse for now *)
  | E.Macro, (Use2 (defplace, def_arity, use_arity)) ->
      (match defplace with
       | PlaceLocal -> [`FOREGROUND "pink";]
       | PlaceSameDir -> [`FOREGROUND "LightPink";]
       | PlaceExternal ->
           (match use_arity with
            | MultiUse -> [`FOREGROUND "PaleVioletRed"]

            | LotsOfUse | HugeUse | SomeUse -> [`FOREGROUND "salmon";]

            | UniqueUse -> [`FOREGROUND "yellow"]
            | NoUse -> [`FOREGROUND "IndianRed";]
           )
       | NoInfoPlace -> [`FOREGROUND "pink1";]

      ) @ info_of_def_arity def_arity

  | E.Method, (Use2 _) -> [`FOREGROUND "gold3";]

  | E.Class, (Use2 _) -> [`FOREGROUND "chartreuse4"] @ info_of_usedef (Use)

  (* this is for codemap completion search box *)
  | E.Package, _ -> [`FOREGROUND "IndianRed"]
  | E.File, _ -> [`FOREGROUND "black"]
  (* see dircolors.ml *)
  | E.Dir, _ -> [`FOREGROUND "CornFlowerBlue"]
  | E.MultiDirs, _ -> [`FOREGROUND "DarkSlateBlue"]

  | _ ->
      failwith (spf "info_of_entity_kind_and_usedef2: missing case for '%s'"
                  (Entity_code.string_of_entity_kind kind))



(*****************************************************************************)
(* Main entry point *)
(*****************************************************************************)

(* pad taste *)
let info_of_category = function

  (* `FAMILY "-misc-*-*-*-*-20-*-*-*-*-*-*"*)
  (* `FONT "-misc-fixed-bold-r-normal--13-100-100-100-c-70-iso8859-1" *)

  (* background *)
  | BackGround -> [`BACKGROUND "DarkSlateGray"]
  | ForeGround -> [`FOREGROUND "wheat";]

  | NotParsed -> [`BACKGROUND "grey42" (*"lightgray"*)]
  | NoType ->    [`BACKGROUND "DimGray"]
  | Passed ->    [`BACKGROUND "DarkSlateGray4"]
  | Expanded ->  [`BACKGROUND "red"]
  | Error ->     [`BACKGROUND "red2"]

  (* a flashy one that hurts the eye :) *)
  | BadSmell -> [`FOREGROUND "magenta"]

  | UseOfRef -> [`FOREGROUND "magenta"]

  | PointerCall ->
      [`FOREGROUND "firebrick";
       `WEIGHT `BOLD;
       `SCALE `XX_LARGE;
      ]

  | ParameterRef -> [`FOREGROUND "magenta"]
  | CallByRef ->
      [`FOREGROUND "orange";
       `WEIGHT `BOLD;
       `SCALE `XX_LARGE;
      ]
  | IdentUnknown ->   [`FOREGROUND "red";]

  (* searches, background *)
  | MatchGlimpse -> [`BACKGROUND "grey46"]
  | MatchSmPL ->    [`BACKGROUND "ForestGreen"]

  | MatchParent -> [`BACKGROUND "blue"]

  | MatchSmPLPositif -> [`BACKGROUND "ForestGreen"]
  | MatchSmPLNegatif -> [`BACKGROUND "red"]


  (* foreground *)
  | Comment -> [`FOREGROUND "gray";]

  | CommentSection0 -> [`FOREGROUND "coral";]
  | CommentSection1 -> [`FOREGROUND "orange";]
  | CommentSection2 -> [`FOREGROUND "LimeGreen";]
  | CommentSection3 -> [`FOREGROUND "LightBlue3";]
  | CommentSection4 -> [`FOREGROUND "gray";]

  | CommentEstet -> [`FOREGROUND "gray";]
  | CommentCopyright -> [`FOREGROUND "DimGray";]
  | CommentSyncweb -> [`FOREGROUND "DimGray";]
  | CommentImportance0 -> [`FOREGROUND "DimGray";]
  | CommentImportance1 -> [`FOREGROUND "gray45";]
  | CommentImportance2 -> [`FOREGROUND "gray";]
  | CommentImportance3 -> [`FOREGROUND "red";]


  (* entities *)
  | Entity (kind, defkind) ->
      info_of_entity_kind_and_usedef2 kind defkind

  | FunctionDecl _ -> [`FOREGROUND "gold2";
                       `WEIGHT `BOLD;`STYLE `ITALIC; `SCALE `MEDIUM;
                      ]

  | Parameter usedef -> [`FOREGROUND "SteelBlue2";] @ info_of_usedef usedef
  | Local usedef  ->    [`FOREGROUND "SkyBlue1";] @ info_of_usedef usedef

  (* | FunCallMultiDef ->[`FOREGROUND "LightGoldenrod";] *)

  | StaticMethod (Def2 _) -> [`FOREGROUND "gold3";
                              `WEIGHT `BOLD; `SCALE `MEDIUM;
                             ]
  | StaticMethod (Use2 _) -> [`FOREGROUND "gold3";
                              `WEIGHT `BOLD; `SCALE `MEDIUM;
                             ]


  | TypeVoid -> [`FOREGROUND "LimeGreen";]
  | TypeInt ->  [`FOREGROUND "chartreuse";]

  | ConstructorMatch _ -> [`FOREGROUND "pink1";]
  | FunctionEquation -> [`FOREGROUND "LightSkyBlue";]

  | StructName usedef -> [`FOREGROUND "YellowGreen"] @ info_of_usedef usedef
  | EnumName usedef -> [`FOREGROUND "YellowGreen"] @ info_of_usedef usedef


  | Ifdef -> [`FOREGROUND "chocolate";]
  | Include -> [`FOREGROUND "DarkOrange2";]
  | IncludeFilePath -> [`FOREGROUND "SpringGreen3";]
  | Define -> [`FOREGROUND "DarkOrange2";]
  | CppOther -> [`FOREGROUND "DarkOrange2";]

  | Attribute -> [`FOREGROUND "DarkOrange2";]


  | Keyword -> [`FOREGROUND "orange";]
  | Builtin -> [`FOREGROUND "salmon";]

  | BuiltinCommentColor -> [`FOREGROUND "gray";]
  | BuiltinBoolean -> [`FOREGROUND "pink";]

  | KeywordConditional -> [`FOREGROUND "DarkOrange";]
  | KeywordLoop -> [`FOREGROUND "sienna1";]

  | KeywordExn -> [`FOREGROUND "orchid";]
  | KeywordObject -> [`FOREGROUND "aquamarine3";]
  | KeywordModule -> [`FOREGROUND "chocolate";]
  | KeywordConcurrency -> [`FOREGROUND "turquoise";]

  | Number -> [`FOREGROUND "yellow3";]
  | Boolean -> [`FOREGROUND "pink3";]
  | String -> [`FOREGROUND "MediumSeaGreen";]
  | Regexp -> [`FOREGROUND "green3";]
  | Atom -> [`FOREGROUND "misty rose";]
  | Null -> [`FOREGROUND "cyan3";]


  | CommentWordImportantNotion ->
      [`FOREGROUND "red"; `SCALE `LARGE;`UNDERLINE `SINGLE; ]
  | CommentWordImportantModal ->
      [`FOREGROUND "green"; `SCALE `LARGE; `UNDERLINE `SINGLE;]

  | Punctuation -> [`FOREGROUND "cyan";]

  | Operator -> [`FOREGROUND "DeepSkyBlue3";] (* could do better ? *)

  | (Label Def) -> [`FOREGROUND "cyan";]
  | (Label Use) -> [`FOREGROUND "CornflowerBlue";]


  (* to be consistent with Archi_code.Ui color *)
  | EmbededHtml -> [`FOREGROUND "RosyBrown"]
  | EmbededHtmlAttr ->[`FOREGROUND "burlywood3"]

  | EmbededUrl ->
      (* yellow-like color, like function, because it's often
       * used as a method call in method programming
      *)
      [`FOREGROUND "DarkGoldenrod2"]

  | EmbededCode ->   [`FOREGROUND "yellow3"]
  | EmbededStyle ->  [`FOREGROUND "peru"]
  | Verbatim ->      [`FOREGROUND "plum"]

  | GrammarRule ->   [`FOREGROUND "plum"]

  | Normal -> [`FOREGROUND "wheat";]

(*****************************************************************************)
(* Generic helpers *)
(*****************************************************************************)

let arity_ids ids  =
  match ids with
  | [] -> NoDef
  | [_] -> UniqueDef
  | [_;_] -> DoubleDef
  | _::_::_::_ -> MultiDef

let rewrap_arity_def2_category arity categ =
  match categ with
  | Entity (kind, (Def2 _)) -> Entity (kind, (Def2 arity))
  | FunctionDecl _ ->  FunctionDecl arity
  | StaticMethod (Def2 _) -> StaticMethod (Def2 arity)
  | _ -> failwith "not a Def2-kind categoriy"
