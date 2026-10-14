<?hh
// ERROR: match
$input |> $$->include("a");
$input |> $$->require("a");
$input |> $$->exclude("a");
