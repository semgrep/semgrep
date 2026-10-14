<?hh
function pure()[]: void {}
function effects()[io, rand]: void {}
function poly((function()[io]: void) $f)[ctx $f]: void {}
function scoped()[C::C, policied<Foo>]: void {}
type Callback = (function(int)[io]: void);
type PlainCallback = (function (int): void);
$lambda = (int $x)[io]: int ==> $x;
$empty = ()[] ==> {};
$closure = function()[io]: void {};
class Methods { public function read()[io]: void {} }
