local a = ({1, ["foo"] = function(self, arg) return {self[1], 1, 2, 3, baz = arg} end}):foo(12).baz
local b = (function(a, b) return "test" .. tostring(a) .. b end)(42, "foo")
