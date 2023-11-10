defmodule Test do

  def test1() do
    #ruleid: taint-maturity
    sink("tainted")
  end

  def test2() do
    x = "safe"
    a = x
    {x, dont_care} = {"tainted", "safe"}
    [y, dont_care] = [x, "safe"]
    z = y
    #ruleid: taint-maturity
    sink(z)
    #OK: taint-maturity
    sink(a)
    #OK: taint-maturity
    safe(z)
  end

  def test3() do
    #OK: taint-maturity
    sink(sanitize("tainted"))
  end

  def test4() do
    x = "tainted"
    x = sanitize(x)
    #OK: taint-maturity
    sink(x)
  end

end
