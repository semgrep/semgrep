<?php

$x = ((function (int): void) $x) ==> {};
$x = ($x): /**/ int ==> {};
$x = ($x): (function (int): int) /**/ ==> {};
$x = ($x): Vector<Vector<(function (int): int)>> ==> {};
$x = ($x): Vector<Vector<int>> ==> {};

$x = true ? (int $x): int ==> {} : false;
$x = () ==> {};
$x = $x /* test */ ==> {};

$a = (int $y) /**/ : void ==> {};
$b = async (int $y): Awaitable<void> ==> {};
$c = async (int $y, int $z) ==> {};
$d = (int $y, int $z): void ==> {};
$e = (Vector<int> $x) ==> {};
$e = (Vector<Vector<int>> $x) ==> {};
$f = ($x, $y): this ==> {};
$g = ((int, int) $y) ==> {};
$h = ((int, int) $y, Vector<int> $z) : (int, int) ==> {};
