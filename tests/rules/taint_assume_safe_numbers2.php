<?php

// ok:taint-assume-safe-numbers
sink((int) source());

// ok:taint-assume-safe-numbers
sink(source() * 3);
// ok:taint-assume-safe-numbers
sink(source() - 1);

$i = (int) source();
// ok:taint-assume-safe-numbers
sink($i);

$i = 3;
// ok:taint-assume-safe-numbers
sink(source() * $i);

// ok:taint-assume-safe-numbers
sink((float) source());
