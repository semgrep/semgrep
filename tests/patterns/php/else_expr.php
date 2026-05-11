<?php

if (true) {
    echo "HAI";
}

if (true) {}

// MATCH:
if (true) {} else {}


if (true):
    echo "HAI";
endif;

if (true):
    // empty
endif;


// MATCH:
if (true):
else:
    // empty
endif;
