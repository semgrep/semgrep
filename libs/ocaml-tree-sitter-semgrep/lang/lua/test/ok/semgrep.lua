if next(msg) == nil then ... end
__index = {
  next = function(self) ... end,
}
local t = {
  self = swim_self,
}
