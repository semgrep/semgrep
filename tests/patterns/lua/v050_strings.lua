-- ERROR: match
sink("hello", value)
-- ERROR: match
sink('hello', value)
-- ERROR: match
sink([[hello]], value)
-- ERROR: match
sink([=[hello]=], value)
sink('goodbye', value)
