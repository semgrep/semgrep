-- ERROR: match
function obj:run(x) return x end
function obj.run(x) return x end
function obj:other(x) return x end
