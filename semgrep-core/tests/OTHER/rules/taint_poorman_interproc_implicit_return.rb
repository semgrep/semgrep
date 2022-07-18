def foo
  user_input
end

def test
  # Poor-man's interpocedural analysis will detect that `foo` is
  # tainted
  x = foo
  #ruleid: test
  sink(x)
end
