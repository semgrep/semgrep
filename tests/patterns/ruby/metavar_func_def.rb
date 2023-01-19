# ERROR:
def foo()
  foo(1,2)
end

# ERROR:
def bar(bar1, bar2, bar3)
  foo(1,2)
  bar(1,2,3)
end

