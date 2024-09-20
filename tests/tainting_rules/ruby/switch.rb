def f()
  i = source()
  #ruleid: test-switch
  sink(i)
  case x
  when 0
    i = sanitize(i)
    #OK:
    sink(i)
  when 1
    unrelated_call()
    #ruleid: test-switch
    sink(i)
  else
    unrelated_call()
    #ruleid: test-switch
    sink(i)
  end
end
