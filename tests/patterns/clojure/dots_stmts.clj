(defun foo()
  ;TODO: need this extra parenthesis to make this test works for now
  (
    ;ERROR:
    (setq user_data (get()))
    (print "do stuff")
    (foobar())
    (eval user_data)
  )
)
