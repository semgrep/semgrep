<?php

// MATCH:
f(False);

// MATCH:
f(FALSE);

$secure = false;
// MATCH:
f($secure);

f(true);

f(0);
