<?php

const X = 1;
// this does not work with pfff because :X
// is considered an XHP identifier
echo $a ?:X;
