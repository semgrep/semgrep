<?php

$a = "foo";

// this does not parse with xhpast in hh mode but works with pfff
// because pfff recognizes an xhp identifier as one token and so is
// looking for ':'['a'-'z'...] in which case here a single ':' is
// clearly a colon. xhpast instead is decomposing all the elements of
// an xhp identifier and so here is considering ':' as an T_XHP_COLON
// already, which then make the grammar fails.
echo $a ?: 1;

$a = null;
echo $a ? : 1;

// this does not work with pfff because :X
// is considered an XHP identifier.
//const X = 1;
//echo $a ?:X;
