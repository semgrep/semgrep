<?php

// this should be parsed as 1 ** (2 ** 3) because ** is right associative

echo 1 ** 2 ** 3;

// this is parsed as (1 * 2) * 3 because * is left associative
echo 1 * 2 * 3;
