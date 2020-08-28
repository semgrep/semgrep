<?php
//from http://blog.golemon.com/2007/01/youre-being-lied-to.html

$a = new stdClass;
$b = $a;
$a->foo = 'bar';
var_dump($b);
/* Notice at this point, that $a and $b are,
 * indeed sharing the same object instance.
 * This is their reference-like behavior at work.
 */

$a = 'baz';
var_dump($b);

/* Notice now, that $b is still that original object.
 * Had it been an actual reference with $a,
 * it would have changed to a simple string as well.
 */
