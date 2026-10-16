next({1, 2, 3})

local t = {bar = 42}

function t:foo()
   print(self.bar)
   self = {1, 2, 3}
end

_G = {}
_G.foo = {}
