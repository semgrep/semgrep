;(***********************************************************************)
;(*                                                                     *)
;(*                           Objective Caml                            *)
;(*                                                                     *)
;(*                Jacques Garrigue and Ian T Zimmerman                 *)
;(*                                                                     *)
;(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id: caml-hilit.el 6612 2004-08-20 17:04:35Z doligez $ *)

; Highlighting patterns for hilit19 under caml-mode

; defined also in caml.el
(defvar caml-quote-char "'"
  "*Quote for character constants. \"'\" for Objective Caml, \"`\" for Caml-Light.")

(defconst caml-mode-patterns
  (list
;comments
   '("\\(^\\|[^\"]\\)\\((\\*[^*]*\\*+\\([^)*][^*]*\\*+\\)*)\\)"
     2 comment)
;string
   (list 'hilit-string-find (string-to-char caml-quote-char) 'string)
   (list (concat caml-quote-char "\\(\\\\\\([ntbr" caml-quote-char "\\]\\|"
                 "[0-9][0-9][0-9]\\)\\|.\\)" caml-quote-char)
         nil
         'string)
;labels
   '("\\(\\([~?]\\|\\<\\)[a-z][a-zA-Z0-9_']*:\\)[^:=]" 1 brown)
   '("[~?][ (]*[a-z][a-zA-Z0-9_']*" nil brown)
;modules
   '("\\<\\(assert\\|open\\|include\\)\\>" nil brown)
   '("`?\\<[A-Z][A-Za-z0-9_\']*\\>" nil MidnightBlue)
;definition
   (list (concat
          "\\<\\(a\\(nd\\|s\\)\\|c\\(onstraint\\|lass\\)"
          "\\|ex\\(ception\\|ternal\\)\\|fun\\(ct\\(ion\\|or\\)\\)?"
          "\\|in\\(herit\\)?\\|let\\|m\\(ethod\\|utable\\|odule\\)"
          "\\|of\\|p\\(arser\\|rivate\\)\\|rec\\|type"
          "\\|v\\(al\\|irtual\\)\\)\\>")
         nil 'ForestGreen)
;blocking
   '("\\<\\(object\\|struct\\|sig\\|begin\\|end\\)\\>" 2 include)
;control
   (list (concat
          "\\<\\(do\\(ne\\|wnto\\)?\\|else\\|for\\|i\\(f\\|gnore\\)"
          "\\|lazy\\|match\\|new\\|or\\|t\\(hen\\|o\\|ry\\)"
          "\\|w\\(h\\(en\\|ile\\)\\|ith\\)\\)\\>"
          "\\|\|\\|->\\|&\\|#")
         nil 'keyword)
   '(";" nil struct))
  "Hilit19 patterns used for Caml mode")

(hilit-set-mode-patterns 'caml-mode caml-mode-patterns)
(hilit-set-mode-patterns
 'inferior-caml-mode
 (append
  (list
;inferior
   '("^[#-]"    nil     firebrick))
  caml-mode-patterns))

(provide 'caml-hilit)
