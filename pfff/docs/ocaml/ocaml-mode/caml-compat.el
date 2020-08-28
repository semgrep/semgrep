;(***********************************************************************)
;(*                                                                     *)
;(*                           Objective Caml                            *)
;(*                                                                     *)
;(*           Xavier Leroy, projet Cristal, INRIA Rocquencourt          *)
;(*                                                                     *)
;(*  Copyright 1998 Institut National de Recherche en Informatique et   *)
;(*  en Automatique.  All rights reserved.  This file is distributed    *)
;(*  under the terms of the GNU General Public License.                 *)
;(*                                                                     *)
;(***********************************************************************)

;(* $Id: caml-compat.el 9547 2010-01-22 12:48:24Z doligez $ *)

;; function definitions for old versions of emacs

;; indent-line-to

(if (not (fboundp 'indent-line-to))
    (defun indent-line-to (column)
      "Indent current line to COLUMN.

This function removes or adds spaces and tabs at beginning of line
only if necessary.  It leaves point at end of indentation."
      (if (= (current-indentation) column)
          (back-to-indentation)
        (beginning-of-line 1)
        (delete-horizontal-space)
        (indent-to column))))

;; buffer-substring-no-properties

(cond
 ((fboundp 'buffer-substring-no-properties))
 ((fboundp 'buffer-substring-without-properties)
  (defalias 'buffer-substring-no-properties
    'buffer-substring-without-properties))
 (t
  (defalias 'buffer-substring-no-properties 'buffer-substring)))

(provide 'caml-compat)
