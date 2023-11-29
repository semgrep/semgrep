defmodule Test do

    def test1(c) do
        x = if c do
                "tainted"
            else
                "safe"
            end
        #ruleid: taint-maturity
        sink(x)
    end

    def test1_1(c) do
        x = "safe"
        if c do
            x  = "tainted"
        end
        #todook: taint-maturity
        sink(x)
    end

    def test2(c) do
        x = "tainted"
        x = if c do
                sanitize(x)
            else
                x
            end
        #ruleid: taint-maturity
        sink(x)
        x = sanitize(x)
        #OK: taint-maturity
        sink(x)
    end

    def test3(c) do
        x = "tainted"
        y = if c do
                #ruleid: taint-maturity
                sink(x)
                sanitize(x)
            else
                y = sanitize(x)
            end
        #ruleid: taint-maturity
        sink(x)
        #OK: taint-maturity
        sink(y)
    end

  # TODO: Write some tests with 'unless', 'case', and 'cond'.

end