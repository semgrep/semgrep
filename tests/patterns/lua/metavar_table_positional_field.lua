-- A positional table field uses the synthetic NextArrayIndex internally.
-- A key metavariable must not bind to that synthetic expression.
local t = {1, 2}
