(defun foo()
  ;TODO: I need this extra '(' for now
  ; to have just the 2 statements together in one List
  (
  ;ERROR:
  (setq myfile (open()))
  (close myfile)
  )
)
