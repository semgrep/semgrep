-- ERROR: match
function collect(... args) return args end
function collect(... items) return items end
function collect(args) return args end
