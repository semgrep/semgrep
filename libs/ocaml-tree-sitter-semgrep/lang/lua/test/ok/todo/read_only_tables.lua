local metatable_cache = setmetatable({}, {__mode='k'})

local function make_getter(real_table)
  local function getter(dummy, key)
    local ans=real_table[key]
    if type(ans)=='table' and not metatable_cache[ans] then
      ans = make_read_only(ans)
    end
    return ans
  end
  return getter
end

local function setter(dummy)
  error("attempt to modify read-only table", 2)
end

local function make_pairs(real_table)
  local function pairs()
    local key, value, real_key = nil, nil, nil
    local function nexter()
      key, value = next(real_table, real_key)
      real_key = key
      if type(key)=='table' and not metatable_cache[key] then
	key = make_read_only(key)
      end
      if type(value)=='table' and not metatable_cache[value] then
	value = make_read_only(value)
      end
      return key, value
    end
    return nexter -- values 2 and 3 dummy
  end
  return pairs
end

function make_read_only(t)
  local new={}
  local mt={
    __metatable = "read only table",
    __index = make_getter(t),
    __newindex = setter,
    __pairs = make_pairs(t),
    __type = "read-only table"}
  setmetatable(new, mt)
  metatable_cache[new]=mt
  return new
end

function ropairs(t)
  local mt = metatable_cache[t]
  if mt==nil then
    error("bad argument #1 to 'ropairs' (read-only table expected, got " ..
	  type(t) .. ")", 2)
  end
  return mt.__pairs()
end
