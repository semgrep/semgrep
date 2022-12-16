(* Patrick Doane and Gerd Stolpmann
 *
 * Copyright (C) 2001-2006 Patrick Doane and Gerd Stolpmann
 * Copyright (C) 2011 Facebook
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

(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * src: most of the code in this file comes from ocamlnet/netstring/.
 * The original CVS ID is:
 * $Id: nethtml.ml 1296 2009-11-18 13:27:41Z ChriS $
 * I've removed the use of open variants and use simple variants.
 * I've also removed the helper functions for the relax_dtd.
 *)

(*****************************************************************************)
(* Types *)
(*****************************************************************************)

(*
 * From Gerd in march on the caml mailing list:
 * "http://www.w3.org/TR/1999/REC-html401-19991224. You will see there that
 * most HTML elements are either an inline element, a block element, or
 * both ("flow" element). The grammar of HTML is described in terms of
 * these classes. For instance, a P tag (paragraph) is a block element and
 * contains block elements whereas B (bold) is an inline element and
 * contains inline elements. From this follows that you cannot put a P
 * inside a B: <B><P>something</P></B> is illegal.
 *
 * The parser needs this information to resolve such input, i.e. do
 * something with bad HTML. As HTML allows tag minimization (many end tags
 * can be omitted), the parser can read this as: <B></B><P>something</P>
 * (and the </B> in the input is ignored).
 *
 * If all start and all end tags are written out, changing the
 * simplified_dtd does not make any difference."
 *)

(** We need a type that declares how to handle the various tags.
 * This is called a "simplified DTD", as it is derived from SGML DTDs,
 * but simplified to the extent used in the HTML definition.
*)

(** Element classes are a property used in the HTML DTD. For our purposes,
 * we define element classes simply as an enumeration:
 * - [`Inline] is the class of inline HTML elements
 * - [`Block] is the class of block HTML elements
 * - [`Essential_block] is a sub-class of [`Block] with the additional
 *   property that every start tag must be explicitly ended
 * - [`None] means that the members of the class are neither block nor
 *   inline elements, but have to be handled specially
 * - [`Everywhere] means that the members of the class can occur everywhere,
 *   regardless of whether a constraint allows it or not.
*)
type element_class =
  | Inline
  | Block
  | Essential_block
  | None
  | Everywhere

(** Model constraints define the possible sub elements of an element:
 * - [`Inline]: The sub elements must belong to the class [`Inline]
 * - [`Block]: The sub elements must be members of the classes [`Block] or
 *   [`Essential_block]
 * - [`Flow]: The sub elements must belong to the classes [`Inline], [`Block],
 *   or [`Essential_block]
 * - [`Empty]: There are no sub elements
 * - [`Any]: Any sub element is allowed
 * - [`Special]: The element has special content (e.g. [<script>]).
 *   Functionally equivalent to [`Empty]
 * - [`Elements l]: Only these enumerated elements may occur as sub elements
 * - [`Or(m1,m2)]: One of the constraints [m1] or [m2] must hold
 * - [`Except(m1,m2)]: The constraint [m1] must hold, and [m2] must not hold
 * - [`Sub_exclusions(l,m)]: The constraint [m] must hold; furthermore,
 *   the elements enumerated in list [l] are not allowed as direct or
 *   indirect subelements, even if [m] or the model of a subelement would
 *   allow them. The difference to [`Except(m, `Elements l)] is that the
 *   exclusion is inherited to the subelements. The [`Sub_exclusions]
 *   expression must be toplevel, i.e. it must not occur within an [`Or],
 *   [`Except], or another ['Sub_exclusions] expression.
 *
 * Note that the members of the class [`Everywhere] are allowed everywhere,
 * regardless of whether the model constraint allows them or not.
 *
 * Note that certain aspects are not modeled:
 * - [#PCDATA]: We do not specify where PCDATA is allowed and where not.
 * - Order, Number: We do neither specify in which order the sub elements must
 *   occur nor how often they can occur
 * - Inclusions: DTDs may describe that an element extraordinarily
 *   allows a list of elements in all sub elements.
 * - Optional tags: Whether start or end tags can be omitted (to some extent,
 *   this can be expressed with [`Essential_block], however)
*)
type model_constraint =
  | Inline2
  | Block2
  | Flow         (* = `Inline or `Block *)

  | Empty
  | Any
  | Special (* for style and script tags *)
  | Elements of string list  (* Enumeration of allowed elements *)

  | Or of (model_constraint * model_constraint)
  | Except of (model_constraint * model_constraint)
  | Sub_exclusions of (string list * model_constraint)

type simplified_dtd =
  (string * (element_class * model_constraint)) list

(*****************************************************************************)
(* Helpers *)
(*****************************************************************************)

let ( |. ) a b = Or(a,b)
(* let ( -. ) a b = Except(a,b) *)

(*****************************************************************************)
(* The DTD *)
(*****************************************************************************)

let block_elements =
  (* Only used for exclusions *)
  [ "p"; "dl"; "div"; "center"; "noscript"; "noframes"; "blockquote"; "form";
    "isindex"; "hr"; "table"; "fieldset"; "address"; "h1"; "h2"; "h3"; "h4";
    "h5"; "h6"; "pre"; "ul"; "ol"; "dir"; "menu" ]

let html40_dtd =
  [ (* --------- INLINE ELEMENTS ------------ *)
    (* %fontstyle; *)
    "tt",                 (Inline, Inline2);
    "i",                  (Inline, Inline2);
    "b",                  (Inline, Inline2);
    "big",                (Inline, Inline2);
    "small",              (Inline, Inline2);
    (* transitional: *)
    "u",                  (Inline, Inline2);
    "s",                  (Inline, Inline2);
    "strike",             (Inline, Inline2);
    (* %phrase; *)
    "em",                 (Inline, Inline2);
    "strong",             (Inline, Inline2);
    "dfn",                (Inline, Inline2);
    "code",               (Inline, Inline2);
    "samp",               (Inline, Inline2);
    "kbd",                (Inline, Inline2);
    "var",                (Inline, Inline2);
    "cite",               (Inline, Inline2);
    "abbr",               (Inline, Inline2);
    "acronym",            (Inline, Inline2);
    (* %special; *)
    "sup",                (Inline, Inline2);
    "sub",                (Inline, Inline2);
    "span",               (Inline, Inline2);
    "bdo",                (Inline, Inline2);
    "br",                 (Inline, Empty);
    "a",                  (Inline, Sub_exclusions(["a"],Inline2));
    "img",                (Inline, Empty);
    "object",             (Inline, (Flow |. Elements ["param"]));
    "script",             (Inline, Special);
    "map",                (Inline, (Flow |. Elements ["area"]));
    "q",                  (Inline, Inline2);
    (* transitional: *)
    "applet",             (Inline, (Flow |. Elements ["param"]));
    "font",               (Inline, Inline2);
    "basefont",           (Inline, Empty);
    "iframe",             (Inline, Flow);
    (* %formctrl; *)
    "input",              (Inline, Empty);
    "select",             (Inline, Elements ["optgroup"; "option"]);
    "textarea",           (Inline, Elements []);    (* #PCDATA *)
    "label",              (Inline, Sub_exclusions( ["label"],
                                                   Inline2));
    "button",             (Inline, Sub_exclusions( ["a"; "input"; "select";
                                                    "textarea"; "label";
                                                    "button"; "form";
                                                    "fieldset"; "isindex";
                                                    "iframe"],
                                                   Flow));
    (* ------------ BLOCK ELEMENTS ----------*)
    "p",                  (Block, Inline2);
    (* %heading; *)
    "h1",                 (Block, Inline2);
    "h2",                 (Block, Inline2);
    "h3",                 (Block, Inline2);
    "h4",                 (Block, Inline2);
    "h5",                 (Block, Inline2);
    "h6",                 (Block, Inline2);
    (* %list; *)
    "ul",                 (Block, Elements ["li"]);
    "ol",                 (Block, Elements ["li"]);
    (* transitional: *)
    "dir",                (Block, Sub_exclusions( block_elements,
                                                  Elements ["li"]));
    "menu",               (Block, Sub_exclusions( block_elements,
                                                  Elements ["li"]));
    (* %preformatted; *)
    "pre",                (Block, Sub_exclusions( [ "img"; "object"; "applet";
                                                    "big"; "small"; "sub";
                                                    "sup"; "font"; "basefont"],
                                                  Inline2));
    (* other: *)
    "dl",                 (Block, Elements ["dt"; "dd"]);
    "div",                (Block, Flow);
    "noscript",           (Block, Flow);
    "blockquote",         (Block, (Flow |. Elements ["script"]));
    (* strict DTD has Block here *)
    "form",               (Block, Sub_exclusions( ["form"],
                                                  Flow |.
                                                  Elements ["script"]));
    (* strict DTD has Block here *)
    "hr",                 (Block, Empty);
    "table",              (Block, Elements ["caption"; "col"; "colgroup";
                                            "thead"; "tfoot"; "tbody"; "tr"]);
    "fieldset",           (Block, (Flow |. Elements ["legend"]));
    "address",            (Block, Inline2);
    (* transitional: *)
    "center",             (Block, Flow);
    "noframes",           (Block, Flow);
    "isindex",            (Block, Empty);
    (* ------------ OTHER ELEMENTS ----------*)
    "body",               (None, (Flow |. Elements ["script"]));
    (* strict DTD has Block here *)
    "area",               (None, Empty);
    "link",               (None, Empty);
    "param",              (None, Empty);
    "ins",                (Everywhere, Flow);
    "del",                (Everywhere, Flow);
    "dt",                 (None, Inline2);
    "dd",                 (None, Flow);
    "li",                 (None, Flow);
    "optgroup",           (None, Elements ["option"]);
    "option",             (None, Elements []);   (* #PCDATA *)
    "legend",             (None, Inline2);
    "caption",            (None, Inline2);
    "thead",              (None, Elements ["tr"]);
    "tbody",              (None, Elements ["tr"]);
    "tfoot",              (None, Elements ["tr"]);
    "colgroup",           (None, Elements ["col"]);
    "col",                (None, Empty);
    "tr",                 (None, Elements ["th"; "td"]);
    "th",                 (None, Flow);
    "td",                 (None, Flow);
    "head",               (None, Elements ["title"; "base"; "script";
                                           "style"; "meta"; "link";
                                           "object"]);
    "title",              (None, Elements []);   (* #PCDATA *)
    "base",               (None, Empty);
    "meta",               (None, Empty);
    "style",              (None, Special);
    "html",               (None, (Flow |.
                                  Elements ["head";
                                            "title"; "base"; "script";
                                            "style"; "meta"; "link";
                                            "object";
                                            "body"; "frameset"]));
    (* transitional: *)
    "frameset",           (None, Elements ["frameset"; "frame"; "noframes"]);
    "frame",              (None, Empty);
  ]


(** A relaxed version of the HTML 4.0 DTD that matches better common
 * practice. In particular, this DTD additionally allows that inline
 * elements may span blocks. For example,
 * {[ <B>text1 <P>text2 ]}
 * is parsed as
 * {[ <B>text1 <P>text2</P></B> ]}
 * and not as
 * {[ <B>text1 </B><P>text2</P> ]}
 * \- the latter is more correct (and parsed by [html40_dtd]), but is not what
 * users expect.
 *
 * Note that this is still not what many browsers implement. For example,
 * Netscape treats most inline tags specially: [<B>] switches bold on,
 * [</B>] switches bold off. For example,
 * {[ <A href='a'>text1<B>text2<A href='b'>text3 ]}
 * is parsed as
 * {[ <A href='a'>text1<B>text2</B></A><B><A href='b'>text3</A></B> ]}
 * \- there is an extra [B] element around the second anchor! (You can
 * see what Netscape parses by loading a page into the "Composer".)
 * IMHO it is questionable to consider inline tags as switches because
 * this is totally outside of the HTML specification, and browsers may
 * differ in that point.
 *
 * Furthermore, several elements are turned into essential blocks:
 * [TABLE], [UL], [OL], and [DL]. David Fox reported a problem with structures
 * like:
 * {[ <TABLE><TR><TD><TABLE><TR><TD>x</TD></TD></TR></TABLE>y</TD></TR></TABLE> ]}
 * i.e. the [TD] of the inner table has two end tags. Without additional
 * help, the second [</TD>] would close the outer table cell. Because of
 * this problem, tables are now essential meaning that it is not allowed
 * to implicitly add a missing [</TABLE>]; every table element has to
 * be explicitly ended. This rule seems to be what many browsers implement.
*)
(* TODO? val relaxed_html40_dtd : simplified_dtd *)
