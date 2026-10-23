; ERROR: match
(foo :app/name 1)
; ERROR: match
(foo :app/name 2 3)
(foo :other/name 1)
(foo ::app/name 1)
(foo :app/other 1)
