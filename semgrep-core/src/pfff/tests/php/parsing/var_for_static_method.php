<?php

//bill:
//php> final class foobar { public static function foo() { print "hello"; }}
//php> foobar::foo();
//hello
//php> $x = 'foobar';
//php> $x::foo();
//Multiline input has no syntactic completion:
//[11844:0000001:0001] [fb1.8.2] Fatal PHP Parse error: syntax error, unexpected T_PAAMAYIM_NEKUDOTAYIM in Command line code on line 1
//PHP Parse error: syntax error, unexpected T_PAAMAYIM_NEKUDOTAYIM in Command line code on line 1
//php> call_user_func(array($x, 'foo'));
//hello

final class foobar { public static function foo() { print "hello"; }}

foobar::foo();
//hello
$x = 'foobar';
$x::foo();
