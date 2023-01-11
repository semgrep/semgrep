<?php

// from michal:

$x = <tag attr="value with $var" />;

// That is because XHP doesn t treat attributes as encaps strings
// but just as regular strings with no escaping. In particular
// this will completely screw up the parser in pfff (php -l works fine):

$x = <tag attr="value \" />;
