local function foo()
    foo()
    z = x + y
    local a = {1,2,3}
    if (a < b) then max= b end
    local first = function(...)
       return select(1, ...)
    end
    local f = first {}
    return a, z, f
end
