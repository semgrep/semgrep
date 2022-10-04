local function multi_return()
   return 1, "foo", function(...) return select(1, ...) end
end
