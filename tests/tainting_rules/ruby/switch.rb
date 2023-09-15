def f()
  i = source()
  #ERROR:
  sink(i)
  case x
  when 0
    i = sanitize(i)
    #OK:
    sink(i)
  when 1
    unrelated_call()
    #ERROR:
    sink(i)
  else
    unrelated_call()
    #ERROR:
    sink(i)
  end
end
