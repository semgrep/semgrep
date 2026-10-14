<?hh
namespace Example;
namespace\run();
function types(num $n, resource $r): nothing { throw new Exception(); }
class Keywords { const int tuple = 1; const int include = 2; }
$result = $input |> Example::include($$);
$input |> $$->include("a");
$input |> $$->require("a");
