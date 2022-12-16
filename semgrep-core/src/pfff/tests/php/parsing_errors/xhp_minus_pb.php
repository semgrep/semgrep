<?php

// this does not parse correctly with pfff because
// "{$count-2}" is not a valid string in PHP
// (echo "{$count-2}";  does not parse in PHP)
// but xhp translate that ('xhpize -d foo.php') into
// new xhp_fbt__param(array('name' => 'more','number' => '{$count-2}',), array(
// $count-2,), __FILE__, 4)
// which then parse because of the single quote. Nevertheless it's
// arguable a bug ...

is actually a bug

$a =
   <fbt:param name="more" number="{$count-2}">
   {$count-2}
   </fbt:param>
 ;
