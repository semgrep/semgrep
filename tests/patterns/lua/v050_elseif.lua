if a then first() elseif b then
  -- ERROR: match
  second(value)
elseif c then third() else fourth() end
if a then first() elseif b then different(value) else fourth() end
