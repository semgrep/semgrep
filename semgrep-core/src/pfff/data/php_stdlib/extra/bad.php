<?php

// People should not abuse the case insensivity of PHP!

// used in phabricator
const True = 1;
const False = 0;

class STDClass { }
class StdClass { }
// used in reflection/reflection.php
class stdclass { }

function pathInfo($path, $opt = 15) { }
