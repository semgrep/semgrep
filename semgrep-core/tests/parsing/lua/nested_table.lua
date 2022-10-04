t = {
   1, 2, {10, 20, 30}, {{"foo" ,"bar", "baz"}}, 3,
   fn = function() return 42 end,
   [0] = 0
}

t.fn()
t:fn()
