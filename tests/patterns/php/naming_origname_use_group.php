<?php

use function Foo\Baz\{fn_a, fn_b, fn_c};

fn_a();
// ERROR: match
fn_b();
fn_c();
