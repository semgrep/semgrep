 def divide_by_zero
    # ruleid: divide-by-zero
    3/0
    # ruleid: divide-by-zero
    oops = 4/0
    variable = 3
    # ruleid: divide-by-zero
    oops = variable / 0
    # ruleid: divide-by-zero
    zero = 0
    bad = variable/zero
 end
