; Metadata is ignored, but its reader form must parse without losing the body.
; ERROR: match
^#=(keyword "a") [1 2]
; ERROR: match
#^#=(keyword "a") [3 4]
; ERROR: match
[5]
(5)
