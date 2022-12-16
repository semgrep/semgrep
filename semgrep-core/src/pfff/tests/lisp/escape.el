     ((string= modifier "*")
      (list '* elem))
     ((string= modifier "?")
      (list '\? elem))


(defconst woman-escaped-escape-char ?
  ;; An arbitrary unused control character
  "Internal character representation of escaped escape characters.")
